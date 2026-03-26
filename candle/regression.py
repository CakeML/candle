"""
Simple Python script to test Candle.

Note that this script assumes passwordless sudo for criu, which can be
achieved by:
1. Calling visudo
2. Adding
   user ALL = (root) NOPASSWD: /usr/bin/criu
   to it, taking care to replace user with the proper username.

No claims are made on whether this a good idea in regards to security.

The reason criu is used, is because DMTCP segfaulted on my machine.
However, I suspect that that might have been due to the packaged
version being outdated, so it is worth reconsidering, as it appears
that DMTCP supports one checkpoint to be restored multiple times
simulatenously, which would make parallelism during testing easier.
"""
import sys
import pexpect
import time
import subprocess
import os
import argparse
from dataclasses import dataclass
from enum import Enum
from pathlib import Path

# <root>/candle/candle-regression.py
CANDLE_ROOT = Path(__file__).resolve().parent.parent

# ---------------------------------------------------------------------------
# Exceptions
# ---------------------------------------------------------------------------

class StartFailure(Exception):
    """Starting Candle failed (pre-boot)."""


class BootFailure(Exception):
    """Booting Candle failed (pre-boot)."""


class LoadFailure(Exception):
    """Loading a file failed."""



# ---------------------------------------------------------------------------
# Test status and result
# ---------------------------------------------------------------------------

class TestStatus(Enum):
    PASS = "PASS"
    FAIL = "FAIL"
    TIMEOUT = "TIMEOUT"

@dataclass
class TestResult:
    name: str
    status: TestStatus
    elapsed: float = 0.0
    error_message: str = ""


TESTS = [
    "100/arithmetic",
    "100/cantor",
    "100/konigsberg",
    "100/gcd",
    "100/wilson",
    "100/combinations",
    "100/ratcountable",
    "100/euler",
    "100/lhopital",
    "100/stirling",
    "100/liouville",
]


# ---------------------------------------------------------------------------
# CandleREPL
# ---------------------------------------------------------------------------

class CandleREPL:
    def __init__(self, restore=False):
        # Easier to assume that we are in the candle directory for now.
        os.chdir(CANDLE_ROOT)

        # Indicates whether we are a restored instance. In that case, I suspect
        # that the pid of self.process might be of sudo criu restore instead of
        # Candle.
        self._cake_pid = None

        self._logfile = sys.stdout
        self._checkpoint_dir = str(CANDLE_ROOT / "checkpoint")
        self._pidfile_name = "cake.pid"

        self.load_stack = []
        self.last_val = None

        if restore:
            self.restore()
        else:
            try:
                self.process = pexpect.spawn(
                    str(CANDLE_ROOT / "candle.sh"),
                    encoding='utf-8',
                    logfile=self._logfile
                )
            except Exception as e:
                raise StartFailure from e
            try:
                self._check_boot()
            except BootFailure:
                self.kill()
                raise

    def _pidfile(self):
        return os.path.join(self._checkpoint_dir, self._pidfile_name)

    def _check_boot(self):
        try:
            index = self.process.expect([
                r'\n# ',
                r'\n(ERROR: .+)',
                pexpect.TIMEOUT,
                pexpect.EOF,
            ])
        except Exception as e:
            raise BootFailure from e

        if index != 0:
            reasons = {
                1: str(self._get_match(1)),
                2: "Timeout",
                3: "Process exited unexpectedly"
            }
            raise BootFailure(reasons[index])

    def _get_match(self, idx):
        return self.process.match.group(idx)

    def load_stack_str(self):
        f"[while loading: {' > '.join(self.load_stack)}]"

    def _check_output(self, timeout=600):
        try:
            index = self.process.expect([
                r'\n\- Loading (\S+)',
                r'\nval (\w+) =',
                r'\n(ERROR: .+)',
                r'\n(Parsing failed)',
                r'\n(EXCEPTION: .+)',
                r'\n\- Finished loading (\S+)',
                pexpect.TIMEOUT,
                pexpect.EOF,
            ], timeout=timeout)
        except Exception as e:
            raise LoadFailure from e

        match index:
            case 0:
                dependency = self._get_match(1)
                self.load_stack.append(dependency)
            case 1:
                self.last_val = self._get_match(1)
            case 2 | 3 | 4:
                raise LoadFailure(f"{self._get_match(1)} {self.load_stack_str()}")
            case 5:
                finished = self._get_match(1)
                expected = self.load_stack.pop()
                assert finished == expected, f'Expected to finish loading {expected}. Actual: {finished}'
            case 6:
                raise LoadFailure(f"Timeout waiting for output {self.load_stack_str()}")
            case 7:
                raise LoadFailure(f"Process exited unexpectedly {self.load_stack_str()}")
            case _:
                assert False, "Unreachable: Did you add a new case in _check_output?"

    def load(self, file, timeout=600):
        self.process.sendline(f'#use "{file}";;')
        self._check_output(timeout=timeout)

        while self.load_stack:
            self._check_output(timeout=timeout)

    def kill(self):
        self.process.close(force=True)

        # We need to make sure that the Candle process is killed and reaped,
        # as criu tries to restore into the same pid (I think),
        # which will fail if the pid is occupied.
        # My understanding is that after restoring at least Candle is a process
        # group that is not the child of this process. The former motivates the
        # g (group), and the latter is why we have the loop with killpg.
        if self._cake_pid:
            subprocess.run(["pkill", "-9", "-g", str(self._cake_pid)])
            while True:
                try:
                    os.killpg(self._cake_pid, 0)
                except ProcessLookupError:
                    return
                time.sleep(0.1)

    def dump(self):
        os.makedirs(self._checkpoint_dir, exist_ok=True)

        # We are not in the situation where Candle has been restored, so the
        # pid of process is correct. We also save it for future restoring.
        if not self._cake_pid:
            pid = self.process.pid
            with open(self._pidfile(), "w") as f:
                f.write(str(pid))
        else:
            pid = self._cake_pid

        subprocess.run(
            ["sudo", "criu", "dump", "-D", self._checkpoint_dir, "-t", str(pid), "--shell-job"],
            check=True,
        )

        # If I remember correctly, this is for making sure the dumped process
        # gets reaped. I think.
        self.process.wait()

    def restore(self):
        self.process = pexpect.spawn(
            "sudo", ["criu", "restore", "-D", self._checkpoint_dir, "--shell-job"],
            encoding='utf-8',
            logfile=self._logfile,
        )
        # Assumption: CRIU restores processes with their original PIDs.
        # Read the cake PID (saved during dump) so kill() can target it.
        self._cake_pid = int(open(self._pidfile()).read().strip())


# ---------------------------------------------------------------------------
# Reporter
# ---------------------------------------------------------------------------

class Reporter:
    STATUS_SYMBOLS = {
        TestStatus.PASS:    "PASS",
        TestStatus.FAIL:    "FAIL",
        TestStatus.TIMEOUT: "TIME",
    }

    @staticmethod
    def print_summary(results):
        # Header
        print()
        print(f"{'Test':<40} {'Status':>6}  {'Time':>8}")
        print("-" * 58)

        for r in results:
            elapsed_str = f"{r.elapsed:.1f}s"
            sym = Reporter.STATUS_SYMBOLS[r.status]
            print(f"{r.name:<40} {sym:>6}  {elapsed_str:>8}")

        # Footer
        counts = {}
        for s in TestStatus:
            c = sum(1 for r in results if r.status == s)
            if c:
                counts[s] = c

        print("-" * 58)
        parts = [f"{s.value}: {c}" for s, c in counts.items()]
        print(f"Total: {len(results)}  |  " + "  ".join(parts))

        # Highlight problems
        failures = [r for r in results if r.status in (TestStatus.FAIL, TestStatus.TIMEOUT)]

        if failures:
            print()
            print("FAILURES:")
            for r in failures:
                msg = f"  {r.name}: {r.status.value}"
                if r.error_message:
                    msg += f" — {r.error_message}"
                print(msg)


# ---------------------------------------------------------------------------
# TestRunner
# ---------------------------------------------------------------------------

class TestRunner:
    def __init__(self, timeout=600, fail_fast=False):
        self.timeout = timeout
        self.fail_fast = fail_fast

    def setup(self, reuse_checkpoint=False):
        """Start candle, load hol.ml, and dump a checkpoint."""
        checkpoint_path = CANDLE_ROOT / "checkpoint"
        if reuse_checkpoint and os.path.isdir(checkpoint_path):
            print("Reusing existing checkpoint.")
            return

        print("Starting candle and loading hol.ml...")
        repl = CandleREPL()
        try:
            repl.load("hol.ml", timeout=3600)
            print("hol.ml loaded. Dumping checkpoint...")
            repl.dump()
            print("Checkpoint created.")
        except Exception:
            repl.kill()
            raise

    def run_test(self, name):
        """Restore from checkpoint, run a single test, return TestResult."""
        start = time.perf_counter()

        try:
            repl = CandleREPL(restore=True)
        except Exception as e:
            elapsed = time.perf_counter() - start
            return TestResult(
                name=name, status=TestStatus.FAIL,
                elapsed=elapsed, error_message=f"Restore failed: {e}",
            )

        try:
            repl.load(f"{name}.ml", timeout=self.timeout)
            elapsed = time.perf_counter() - start

            return TestResult(name=name, status=TestStatus.PASS, elapsed=elapsed)

        except LoadFailure as e:
            elapsed = time.perf_counter() - start
            err = str(e)
            if repl.load_stack:
                err += f" {repl.load_stack_str()}"
            if repl.last_val:
                err += f" (last val: {repl.last_val})"
            if "Timeout" in str(e):
                status = TestStatus.TIMEOUT

            else:
                status = TestStatus.FAIL
            return TestResult(name=name, status=status, elapsed=elapsed, error_message=err)

        except pexpect.TIMEOUT:
            elapsed = time.perf_counter() - start
            err = "Timeout"
            if repl.load_stack:
                err += f" {repl.load_stack_str()}"
            if repl.last_val:
                err += f" (last val: {repl.last_val})"
            else:
                status = TestStatus.TIMEOUT
            return TestResult(
                name=name, status=status,
                elapsed=elapsed, error_message=err,
            )

        except Exception as e:
            elapsed = time.perf_counter() - start
            err = str(e)
            if repl.load_stack:
                err += f" {repl.load_stack_str()}"
            if repl.last_val:
                err += f" (last val: {repl.last_val})"
            else:
                status = TestStatus.FAIL
            return TestResult(
                name=name, status=status,
                elapsed=elapsed, error_message=err,
            )

        finally:
            repl.kill()


    def run_all(self, tests):
        """Run all tests, printing progress inline."""
        results = []
        total = len(tests)

        try:
            for i, name in enumerate(tests, 1):
                print(f"[{i}/{total}] {name} ... ", end="", flush=True)
                result = self.run_test(name)
                sym = Reporter.STATUS_SYMBOLS[result.status]
                print(f"{sym} ({result.elapsed:.1f}s)")
                if result.error_message and result.status in (TestStatus.FAIL, TestStatus.TIMEOUT):
                    print(f"         {result.error_message}")
                results.append(result)
                if self.fail_fast and result.status in (TestStatus.FAIL, TestStatus.TIMEOUT):
                    print("Stopping early due to --fail-fast.")
                    break
        except KeyboardInterrupt:
            print("\nInterrupted — showing results so far.")

        return results


# ---------------------------------------------------------------------------
# CLI
# ---------------------------------------------------------------------------

def main():
    parser = argparse.ArgumentParser(
        description="Candle regression test suite",
    )
    parser.add_argument(
        "--reuse-checkpoint", action="store_true",
        help="Skip hol.ml loading if a checkpoint already exists",
    )
    parser.add_argument(
        "--test", nargs="+",
        help="Run specific test(s) by name",
    )
    parser.add_argument(
        "--list", action="store_true",
        help="List available tests and exit",
    )
    parser.add_argument(
        "--fail-fast", action="store_true",
        help="Stop after the first unexpected failure",
    )
    parser.add_argument(
        "--timeout", type=int, default=600,
        help="Per-test timeout in seconds (default: 600)",
    )

    args = parser.parse_args()

    available = TESTS
    if args.list:
        for name in available:
            print(f"  {name}")
        print(f"\n{len(available)} tests")
        return

    # Determine which tests to run
    if args.test:
        tests = args.test
    else:
        tests = list(TESTS)

    runner = TestRunner(
        timeout=args.timeout,
        fail_fast=args.fail_fast,
    )

    # Setup checkpoint
    runner.setup(reuse_checkpoint=args.reuse_checkpoint)

    # Run tests
    results = runner.run_all(tests)

    # Report
    Reporter.print_summary(results)

    # Exit code: 0 if no unexpected failures
    unexpected = [r for r in results if r.status in (TestStatus.FAIL, TestStatus.TIMEOUT)]
    sys.exit(1 if unexpected else 0)


if __name__ == "__main__":
    main()
