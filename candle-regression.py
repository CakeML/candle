"""
Mostly vibe-coded, sigh.
"""
import sys
import pexpect
import time
import subprocess
import os
import signal
import argparse
import json
from dataclasses import dataclass, field, asdict
from enum import Enum
from datetime import datetime

def wait_group(pgid):
    while True:
        try:
            os.killpg(pgid, 0)
        except ProcessLookupError:
            return
        time.sleep(0.1)

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
    SKIP = "SKIP"
    XFAIL = "XFAIL"
    XPASS = "XPASS"


@dataclass
class TestResult:
    name: str
    status: TestStatus
    elapsed: float = 0.0
    error_message: str = ""
    output_log: str = ""


# ---------------------------------------------------------------------------
# Test list — 64 Great 100 Theorems from candletest.mk
# ---------------------------------------------------------------------------

GREAT_100_THEOREMS = [
    "100/arithmetic_geometric_mean",
    "100/arithmetic",
    "100/ballot",
    "100/bernoulli",
    "100/bertrand",
    "100/birthday",
    "100/cantor",
    "100/cayley_hamilton",
    "100/ceva",
    "100/circle",
    "100/chords",
    "100/combinations",
    "100/constructible",
    "100/cosine",
    "100/cubic",
    "100/derangements",
    "100/desargues",
    "100/descartes",
    "100/dirichlet",
    "100/div3",
    "100/divharmonic",
    "100/e_is_transcendental",
    "100/euler",
    "100/feuerbach",
    "100/fourier",
    "100/four_squares",
    "100/friendship",
    "100/fta",
    "100/gcd",
    "100/heron",
    "100/inclusion_exclusion",
    "100/independence",
    "100/isoperimetric",
    "100/isosceles",
    "100/konigsberg",
    "100/lagrange",
    "100/leibniz",
    "100/lhopital",
    "100/liouville",
    "100/minkowski",
    "100/morley",
    "100/pascal",
    "100/perfect",
    "100/pick",
    "100/piseries",
    "100/platonic",
    "100/pnt",
    "100/polyhedron",
    "100/primerecip",
    "100/ptolemy",
    "100/pythagoras",
    "100/quartic",
    "100/ramsey",
    "100/ratcountable",
    "100/realsuncountable",
    "100/reciprocity",
    "100/sqrt",
    "100/stirling",
    "100/subsequence",
    "100/thales",
    "100/transcendence",
    "100/triangular",
    "100/two_squares",
    "100/wilson",
]

# Curated 18-test subset for --quick: maximum code coverage, no binding conflicts.
QUICK_TESTS = [
    # Core tactics (no extra deps)
    "100/arithmetic",       # INDUCT_TAC, ARITH_TAC
    "100/cantor",           # set theory, MESON
    "100/konigsberg",       # graph theory (safe — no standard name redefinitions)
    # Library coverage (one per major library)
    "100/gcd",              # Library/prime.ml
    "100/wilson",           # Library/pocklington.ml
    "100/combinations",     # Library/binomial.ml
    "100/ratcountable",     # Library/card.ml
    "100/euler",            # Library/binary.ml
    "100/lhopital",         # Library/analysis.ml
    "100/stirling",         # Library/transc.ml
    "100/liouville",        # Library/floor.ml + Library/poly.ml
    # Multivariate coverage (one per chain level)
    "100/thales",           # Multivariate/convex.ml chain
    "100/desargues",        # Multivariate/cross.ml
    "100/cosine",           # Multivariate/transcendentals.ml
    "100/descartes",        # Multivariate/realanalysis.ml
    "100/heron",            # Multivariate/measure.ml
    "100/fourier",          # Multivariate/lpspaces.ml
    # Complex coverage
    "100/cubic",            # Complex/complex_transc.ml
]

# Dependency-optimized order for --local mode (needs deduplicates library loads).
LOCAL_ORDER = [
    # Phase 1: No extra deps
    "100/arithmetic", "100/birthday", "100/cantor", "100/divharmonic",
    "100/inclusion_exclusion", "100/konigsberg", "100/quartic", "100/subsequence",
    # Phase 2: Library/prime.ml
    "100/gcd", "100/perfect", "100/fta", "100/two_squares", "100/lagrange",
    # Phase 3: + pocklington.ml
    "100/friendship", "100/div3", "100/wilson", "100/reciprocity",
    # Phase 4: Library/binomial.ml
    "100/combinations", "100/ballot",
    # Phase 5: Library/card.ml
    "100/ratcountable", "100/realsuncountable",
    # Phase 6: Library/binary.ml
    "100/euler",
    # Phase 7: Library/analysis.ml + transc
    "100/lhopital", "100/four_squares", "100/stirling", "100/leibniz",
    "100/bernoulli", "100/derangements", "100/arithmetic_geometric_mean",
    # Phase 8: Library/floor + poly
    "100/liouville",
    # Phase 9: Multivariate/misc + vectors
    "100/triangular", "100/pythagoras",
    # Phase 10: Multivariate/convex.ml
    "100/chords", "100/thales", "100/feuerbach", "100/ceva",
    # Phase 11: Multivariate/cross.ml
    "100/desargues", "100/pascal",
    # Phase 12: Multivariate/complexes.ml
    "100/cayley_hamilton",
    # Phase 13: Multivariate/transcendentals.ml
    "100/ptolemy", "100/cosine", "100/constructible",
    # Phase 14: Multivariate/realanalysis.ml
    "100/descartes",
    # Phase 15: Multivariate/geom.ml
    "100/isosceles", "100/morley",
    # Phase 16: Multivariate/measure.ml
    "100/heron", "100/minkowski", "100/circle",
    # Phase 17: Multivariate/polytope.ml
    "100/polyhedron", "100/pick",
    # Phase 18: Multivariate/lpspaces.ml
    "100/fourier",
    # Phase 19: Multivariate/cauchy.ml
    "100/independence", "100/isoperimetric",
    # Phase 20: Complex
    "100/cubic",
    # Phase 21: Heavy compound (deps already loaded)
    "100/bertrand", "100/primerecip", "100/e_is_transcendental", "100/piseries",
    "100/platonic", "100/dirichlet", "100/pnt", "100/transcendence",
    # Phase 22: ramsey last — redefines standard conversions
    "100/ramsey",
    # sqrt not in candletest.mk phases but is in GREAT_100_THEOREMS
    "100/sqrt",
]

# Tests known to fail — maps test name to reason.
# Tests here that fail get XFAIL; tests here that pass get XPASS.
KNOWN_FAILURES = {}


# ---------------------------------------------------------------------------
# CandleREPL
# ---------------------------------------------------------------------------

class CandleREPL:
    def __init__(self, base, logfile=None):
        os.chdir(base)
        self.base = base
        self.checkpoint_dir = "checkpoint"
        self._logfile = logfile or sys.stdout

        try:
            self.process = pexpect.spawn(
                "./candle", encoding='utf-8', logfile=self._logfile,
            )
        except Exception as e:
            raise StartFailure from e

        try:
            self._check_boot()
        except BootFailure:
            self.kill()
            raise

        self.load_stack = []
        self.last_val = None

    @classmethod
    def from_checkpoint(cls, base_dir, logfile=None):
        """Create a CandleREPL by restoring from an existing checkpoint."""
        obj = object.__new__(cls)
        obj.base = base_dir
        obj.checkpoint_dir = "checkpoint"
        obj._logfile = logfile or sys.stdout
        obj.load_stack = []
        obj.last_val = None
        os.chdir(base_dir)
        obj.restore()
        return obj

    def _check_boot(self):
        try:
            index = self.process.expect([
                r'\n# ',            # 0: REPL prompt → success
                r'\n(ERROR: .+)',   # 1: any boot error
                pexpect.TIMEOUT,
                pexpect.EOF,
            ])
        except Exception as e:
            raise BootFailure from e

        if index != 0:
            reasons = {1: str(self._get_match(1)), 2: "Timeout", 3: "Process exited unexpectedly"}
            raise BootFailure(reasons[index])

    def _get_match(self, idx):
        return self.process.match.group(idx)

    def _set_last_val(self, val):
        self.last_val = (val, time.perf_counter())

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
                self._set_last_val(self._get_match(1))
            case 2 | 3 | 4:
                raise LoadFailure(self._get_match(1))
            case 5:
                finished = self._get_match(1)
                expected = self.load_stack.pop()
                assert finished == expected, f'Expected to finish loading {expected}. Actual: {finished}'
            case 6:
                raise LoadFailure("Timeout waiting for output")
            case 7:
                raise LoadFailure("Process exited unexpectedly")
            case _:
                pass

    def load(self, file, timeout=600):
        self.process.sendline(f'#use "{file}";;')
        self._check_output(timeout=timeout)

        while self.load_stack:
            self._check_output(timeout=timeout)

    def run_loads(self, file, timeout=600):
        """Send loads "file";; — HOL Light's dependency-aware loader."""
        self.process.sendline(f'loads "{file}";;')
        self._check_output(timeout=timeout)

        while self.load_stack:
            self._check_output(timeout=timeout)

    def send_exit(self, timeout=30):
        """Send Cake.Runtime.exit 0;; and wait for the process to terminate."""
        self.process.sendline('Cake.Runtime.exit 0;;')
        self.process.expect(pexpect.EOF, timeout=timeout)

    def wait_for_prompt(self, timeout=30):
        """After restore, send ();; to elicit a prompt from the restored process."""
        self.process.sendline('();;')
        self.process.expect(r'# ', timeout=timeout)

    def kill(self):
        try:
            self.process.close(force=True)
        except Exception:
            pass
        if hasattr(self, '_cake_pid'):
            subprocess.run(["pkill", "-9", "-g", str(self._cake_pid)])
            wait_group(self._cake_pid)

    def dump(self):
        os.makedirs(self.checkpoint_dir, exist_ok=True)
        pid = self.process.pid
        pidfile = os.path.join(self.checkpoint_dir, "cake.pid")
        with open(pidfile, "w") as f:
            f.write(str(pid))
        subprocess.run(
            ["sudo", "criu", "dump", "-D", self.checkpoint_dir, "-t", str(pid), "--shell-job"],
            check=True,
        )
        self.process.wait()

    def restore(self, logfile=None):
        lf = logfile if logfile is not None else self._logfile
        self.process = pexpect.spawn(
            "sudo", ["criu", "restore", "-D", self.checkpoint_dir, "--shell-job"],
            encoding='utf-8',
            logfile=lf,
        )
        # Assumption: CRIU restores processes with their original PIDs.
        # Read the cake PID (saved during dump) so kill() can target it.
        pidfile = os.path.join(self.checkpoint_dir, "cake.pid")
        self._cake_pid = int(open(pidfile).read().strip())


# ---------------------------------------------------------------------------
# Reporter
# ---------------------------------------------------------------------------

class Reporter:
    STATUS_SYMBOLS = {
        TestStatus.PASS:    "PASS",
        TestStatus.FAIL:    "FAIL",
        TestStatus.TIMEOUT: "TIME",
        TestStatus.SKIP:    "SKIP",
        TestStatus.XFAIL:   "XFAIL",
        TestStatus.XPASS:   "XPASS",
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
        xpasses = [r for r in results if r.status == TestStatus.XPASS]

        if failures:
            print()
            print("NEW FAILURES:")
            for r in failures:
                msg = f"  {r.name}: {r.status.value}"
                if r.error_message:
                    msg += f" — {r.error_message}"
                print(msg)

        if xpasses:
            print()
            print("REGRESSIONS (now passing — update KNOWN_FAILURES):")
            for r in xpasses:
                print(f"  {r.name}")

    @staticmethod
    def write_json(results, path):
        counts = {}
        for s in TestStatus:
            c = sum(1 for r in results if r.status == s)
            if c:
                counts[s.value] = c

        data = {
            "timestamp": datetime.now().isoformat(),
            "summary": {
                "total": len(results),
                **counts,
            },
            "tests": [
                {
                    "name": r.name,
                    "status": r.status.value,
                    "elapsed": round(r.elapsed, 2),
                    "error_message": r.error_message,
                }
                for r in results
            ],
        }

        with open(path, "w") as f:
            json.dump(data, f, indent=2)
            f.write("\n")
        print(f"\nJSON results written to {path}")


# ---------------------------------------------------------------------------
# TestRunner
# ---------------------------------------------------------------------------

class TestRunner:
    def __init__(self, base_dir, timeout=600, verbose=False, fail_fast=False):
        self.base_dir = base_dir
        self.timeout = timeout
        self.verbose = verbose
        self.fail_fast = fail_fast

    def setup(self, reuse_checkpoint=False):
        """Start candle, load hol.ml, and dump a checkpoint."""
        checkpoint_path = os.path.join(self.base_dir, "checkpoint")
        if reuse_checkpoint and os.path.isdir(checkpoint_path):
            print("Reusing existing checkpoint.")
            return

        print("Starting candle and loading hol.ml...")
        logfile = sys.stdout if self.verbose else None
        repl = CandleREPL(self.base_dir, logfile=logfile)
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
        logfile = sys.stdout if self.verbose else None
        start = time.perf_counter()

        try:
            repl = CandleREPL.from_checkpoint(self.base_dir, logfile=logfile)
        except Exception as e:
            elapsed = time.perf_counter() - start
            return TestResult(
                name=name, status=TestStatus.FAIL,
                elapsed=elapsed, error_message=f"Restore failed: {e}",
            )

        try:
            repl.wait_for_prompt(timeout=30)
            repl.run_loads(f"{name}.ml", timeout=self.timeout)
            repl.send_exit(timeout=30)
            elapsed = time.perf_counter() - start

            if name in KNOWN_FAILURES:
                return TestResult(name=name, status=TestStatus.XPASS, elapsed=elapsed)
            return TestResult(name=name, status=TestStatus.PASS, elapsed=elapsed)

        except LoadFailure as e:
            elapsed = time.perf_counter() - start
            err = str(e)
            if repl.load_stack:
                err += f" [while loading: {' > '.join(repl.load_stack)}]"
            if repl.last_val:
                err += f" (last val: {repl.last_val[0]})"
            if "Timeout" in str(e):
                status = TestStatus.TIMEOUT
            elif name in KNOWN_FAILURES:
                status = TestStatus.XFAIL
            else:
                status = TestStatus.FAIL
            return TestResult(name=name, status=status, elapsed=elapsed, error_message=err)

        except pexpect.TIMEOUT:
            elapsed = time.perf_counter() - start
            err = "Timeout"
            if repl.load_stack:
                err += f" [while loading: {' > '.join(repl.load_stack)}]"
            if repl.last_val:
                err += f" (last val: {repl.last_val[0]})"
            if name in KNOWN_FAILURES:
                status = TestStatus.XFAIL
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
                err += f" [while loading: {' > '.join(repl.load_stack)}]"
            if repl.last_val:
                err += f" (last val: {repl.last_val[0]})"
            if name in KNOWN_FAILURES:
                status = TestStatus.XFAIL
            else:
                status = TestStatus.FAIL
            return TestResult(
                name=name, status=status,
                elapsed=elapsed, error_message=err,
            )

        finally:
            repl.kill()

    def run_test_local(self, repl, name):
        """Run a single test in an existing REPL session, return TestResult."""
        start = time.perf_counter()

        try:
            repl.run_loads(f"{name}.ml", timeout=self.timeout)
            elapsed = time.perf_counter() - start

            if name in KNOWN_FAILURES:
                return TestResult(name=name, status=TestStatus.XPASS, elapsed=elapsed)
            return TestResult(name=name, status=TestStatus.PASS, elapsed=elapsed)

        except LoadFailure as e:
            elapsed = time.perf_counter() - start
            err = str(e)
            if repl.load_stack:
                err += f" [while loading: {' > '.join(repl.load_stack)}]"
            if repl.last_val:
                err += f" (last val: {repl.last_val[0]})"
            repl.load_stack.clear()
            if "Timeout" in str(e):
                status = TestStatus.TIMEOUT
            elif name in KNOWN_FAILURES:
                status = TestStatus.XFAIL
            else:
                status = TestStatus.FAIL
            return TestResult(name=name, status=status, elapsed=elapsed, error_message=err)

        except pexpect.TIMEOUT:
            elapsed = time.perf_counter() - start
            err = "Timeout"
            if repl.load_stack:
                err += f" [while loading: {' > '.join(repl.load_stack)}]"
            if repl.last_val:
                err += f" (last val: {repl.last_val[0]})"
            repl.load_stack.clear()
            if name in KNOWN_FAILURES:
                status = TestStatus.XFAIL
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
                err += f" [while loading: {' > '.join(repl.load_stack)}]"
            if repl.last_val:
                err += f" (last val: {repl.last_val[0]})"
            repl.load_stack.clear()
            if name in KNOWN_FAILURES:
                status = TestStatus.XFAIL
            else:
                status = TestStatus.FAIL
            return TestResult(
                name=name, status=status,
                elapsed=elapsed, error_message=err,
            )

    def run_all_local(self, tests):
        """Run all tests in a single REPL session (no CRIU)."""
        results = []
        total = len(tests)
        logfile = sys.stdout if self.verbose else None

        print("Starting candle and loading hol.ml...")
        repl = CandleREPL(self.base_dir, logfile=logfile)
        try:
            repl.load("hol.ml", timeout=3600)
            print("hol.ml loaded.")
        except Exception:
            repl.kill()
            raise

        try:
            for i, name in enumerate(tests, 1):
                print(f"[{i}/{total}] {name} ... ", end="", flush=True)
                result = self.run_test_local(repl, name)
                sym = Reporter.STATUS_SYMBOLS[result.status]
                print(f"{sym} ({result.elapsed:.1f}s)")
                if result.error_message and result.status in (TestStatus.FAIL, TestStatus.TIMEOUT):
                    print(f"         {result.error_message}")
                results.append(result)
                if self.fail_fast and result.status in (TestStatus.FAIL, TestStatus.TIMEOUT):
                    print("Stopping early due to --fail-fast.")
                    break
                # After a failure, check the REPL is still alive
                if result.status in (TestStatus.FAIL, TestStatus.TIMEOUT, TestStatus.XFAIL):
                    try:
                        repl.process.sendline('();;')
                        repl.process.expect(r'# ', timeout=30)
                    except Exception:
                        print("REPL unresponsive after failure — aborting remaining tests.")
                        break
        except KeyboardInterrupt:
            print("\nInterrupted — showing results so far.")
        finally:
            repl.kill()

        return results

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
        "-t", "--test", action="append", default=[],
        help="Run specific test(s) by name (can be repeated)",
    )
    parser.add_argument(
        "--json", metavar="FILE",
        help="Write JSON results to FILE",
    )
    parser.add_argument(
        "--list", action="store_true",
        help="List available tests and exit",
    )
    parser.add_argument(
        "-v", "--verbose", action="store_true",
        help="Show verbose REPL output",
    )
    parser.add_argument(
        "--fail-fast", action="store_true",
        help="Stop after the first unexpected failure",
    )
    parser.add_argument(
        "--local", action="store_true",
        help="Run tests in a single REPL session (no CRIU, deduplicates library loads)",
    )
    parser.add_argument(
        "--quick", action="store_true",
        help="Run curated 18-test subset for maximum coverage",
    )
    parser.add_argument(
        "--timeout", type=int, default=600,
        help="Per-test timeout in seconds (default: 600)",
    )
    parser.add_argument(
        "--base-dir", default=os.path.dirname(os.path.abspath(__file__)),
        help="Candle base directory (default: script directory)",
    )

    args = parser.parse_args()

    # Select test list
    available = QUICK_TESTS if args.quick else GREAT_100_THEOREMS

    if args.list:
        for name in available:
            marker = " (KNOWN_FAILURE)" if name in KNOWN_FAILURES else ""
            print(f"  {name}{marker}")
        label = "quick" if args.quick else "available"
        print(f"\n{len(available)} {label} tests")
        return

    # Determine which tests to run
    if args.test:
        tests = []
        for t in args.test:
            if t in GREAT_100_THEOREMS:
                tests.append(t)
            else:
                print(f"Unknown test: {t}", file=sys.stderr)
                sys.exit(1)
    elif args.quick:
        tests = list(QUICK_TESTS)
    else:
        tests = list(GREAT_100_THEOREMS)

    # In local mode without explicit --test, reorder for optimal library dedup
    if args.local and not args.test:
        order = {name: i for i, name in enumerate(LOCAL_ORDER)}
        tests.sort(key=lambda t: order.get(t, len(LOCAL_ORDER)))

    runner = TestRunner(
        base_dir=args.base_dir,
        timeout=args.timeout,
        verbose=args.verbose,
        fail_fast=args.fail_fast,
    )

    if args.local:
        results = runner.run_all_local(tests)
    else:
        # Setup checkpoint
        runner.setup(reuse_checkpoint=args.reuse_checkpoint)

        # Run tests
        results = runner.run_all(tests)

    # Report
    Reporter.print_summary(results)
    if args.json:
        Reporter.write_json(results, args.json)

    # Exit code: 0 if no unexpected failures
    unexpected = [r for r in results if r.status in (TestStatus.FAIL, TestStatus.TIMEOUT, TestStatus.XPASS)]
    sys.exit(1 if unexpected else 0)


if __name__ == "__main__":
    main()
