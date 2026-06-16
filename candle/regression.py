"""
!!! Large amount of LLM generated code !!!

Parallel regression test runner for Candle.

Each test runs in its own fresh Candle process that loads hol.ml and then the
test file(s). There is no checkpointing (no criu/DMTCP, no sudo): running one
process per test, up to -j at a time, is what hides the cost of reloading
hol.ml for every test.

The Candle REPL transcript for each test is written to its own temporary log
file under /tmp (via tempfile.mkstemp), e.g. /tmp/candle-100_arithmetic-XXXX.log.

Two suites are available:
  * REGRESSION - a small subset, run by default.
  * TOP100     - the full "Top 100 theorems" set (from holtest.mk's
                 GREAT_100_THEOREMS), run with --top100.
"""
import sys
import os
import time
import argparse
import tempfile
import concurrent.futures
from dataclasses import dataclass
from enum import Enum
from pathlib import Path

import pexpect

# <root>/candle/regression.py
CANDLE_ROOT = Path(__file__).resolve().parent.parent

# The Top 100 suite needs a larger CakeML heap than the build's default.
# Set via CML_HEAP_SIZE (MB) per candle process; parallelism is capped so the
# combined heap reservation stays within available memory.
TOP100_HEAP_MB = 6000

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
# Test definitions
# ---------------------------------------------------------------------------

class TestStatus(Enum):
    PASS = "PASS"
    FAIL = "FAIL"
    TIMEOUT = "TIMEOUT"


@dataclass(frozen=True)
class Test:
    """A named test, defined by the file(s) it loads (in order) on top of hol.ml."""
    name: str
    files: tuple


def _t(name, *files):
    """Build a Test. With no explicit files, loads "<name>.ml"."""
    return Test(name, files if files else (f"{name}.ml",))


# The default regression subset.
REGRESSION = [
    _t("100/arithmetic"),
    _t("100/cantor"),
    _t("100/konigsberg"),
    _t("100/gcd"),
    _t("100/wilson"),
    _t("100/combinations"),
    _t("100/ratcountable"),
    _t("100/euler"),
    _t("100/lhopital"),
    _t("100/stirling"),
    _t("100/liouville"),
    _t("100/cayley_hamilton"),
]

# The full "Top 100 theorems" suite, mirroring GREAT_100_THEOREMS in holtest.mk.
# bertrand-primerecip is special: primerecip.ml depends on bertrand.ml, so both
# are loaded (in order) in the same session.
TOP100 = [
    _t("100/arithmetic_geometric_mean"),
    _t("100/arithmetic"),
    _t("100/ballot"),
    _t("100/bernoulli"),
    _t("100/bertrand-primerecip", "100/bertrand.ml", "100/primerecip.ml"),
    _t("100/birthday"),
    _t("100/buffon"),
    _t("100/cantor"),
    _t("100/cayley_hamilton"),
    _t("100/ceva"),
    _t("100/circle"),
    _t("100/chords"),
    _t("100/combinations"),
    _t("100/constructible"),
    _t("100/cosine"),
    _t("100/cubedissection"),
    _t("100/cubic"),
    _t("100/derangements"),
    _t("100/desargues"),
    _t("100/descartes"),
    _t("100/dirichlet"),
    _t("100/div3"),
    _t("100/divharmonic"),
    _t("100/e_is_transcendental"),
    _t("100/euler"),
    _t("100/feuerbach"),
    _t("100/fourier"),
    _t("100/four_squares"),
    _t("100/friendship"),
    _t("100/fta"),
    _t("100/gcd"),
    _t("100/green"),
    _t("100/heron"),
    _t("100/isoperimetric"),
    _t("100/inclusion_exclusion"),
    _t("100/independence"),
    _t("100/isosceles"),
    _t("100/konigsberg"),
    _t("100/lagrange"),
    _t("100/leibniz"),
    _t("100/lhopital"),
    _t("100/liouville"),
    _t("100/minkowski"),
    _t("100/morley"),
    _t("100/pascal"),
    _t("100/perfect"),
    _t("100/pick"),
    _t("100/piseries"),
    _t("100/platonic"),
    _t("100/pnt"),
    _t("100/polyhedron"),
    _t("100/ptolemy"),
    _t("100/pythagoras"),
    _t("100/quartic"),
    _t("100/ramsey"),
    _t("100/ratcountable"),
    _t("100/realsuncountable"),
    _t("100/reciprocity"),
    _t("100/stirling"),
    _t("100/subsequence"),
    _t("100/thales"),
    _t("100/transcendence"),
    _t("100/triangular"),
    _t("100/two_squares"),
    _t("100/wilson"),
]

# Lookup by name, so --test can reuse multi-file definitions (e.g. the
# bertrand-primerecip pairing) rather than always assuming "<name>.ml".
BY_NAME = {t.name: t for t in TOP100}


# ---------------------------------------------------------------------------
# CandleREPL
# ---------------------------------------------------------------------------

class CandleREPL:
    def __init__(self, logfile, env=None):
        self._logfile = logfile
        self.load_stack = []
        self.last_val = None

        try:
            self.process = pexpect.spawn(
                str(CANDLE_ROOT / "candle.sh"),
                encoding="utf-8",
                logfile=logfile,
                cwd=str(CANDLE_ROOT),
                env=env,
            )
        except Exception as e:
            raise StartFailure from e

        try:
            self._check_boot()
        except BootFailure:
            self.kill()
            raise

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
                3: "Process exited unexpectedly",
            }
            raise BootFailure(reasons[index])

    def _get_match(self, idx):
        return self.process.match.group(idx)

    def load_stack_str(self):
        return f"[while loading: {' > '.join(self.load_stack)}]"

    def _check_output(self, timeout):
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
                raise LoadFailure(self._get_match(1))
            case 5:
                finished = self._get_match(1)
                expected = self.load_stack.pop()
                assert finished == expected, (
                    f"Expected to finish loading {expected}. Actual: {finished}")
            case 6:
                raise LoadFailure("Timeout waiting for output")
            case 7:
                raise LoadFailure("Process exited unexpectedly")
            case _:
                assert False, "Unreachable: Did you add a new case in _check_output?"

    def load(self, file, timeout):
        self.process.sendline(f'#use "{file}";;')
        self._check_output(timeout=timeout)

        while self.load_stack:
            self._check_output(timeout=timeout)

    def kill(self):
        # No criu restore anymore, so there is no detached process group to
        # chase down: closing the pexpect child (and its candle.sh subtree) is
        # enough.
        self.process.close(force=True)


# ---------------------------------------------------------------------------
# Test result and runner
# ---------------------------------------------------------------------------

@dataclass
class TestResult:
    name: str
    status: TestStatus
    hol_elapsed: float = 0.0
    test_elapsed: float = 0.0
    error_message: str = ""
    log_path: str = ""

    @property
    def total(self):
        return self.hol_elapsed + self.test_elapsed


def _format_error(repl, exc):
    """One-line failure summary: the error, where it happened, last value bound."""
    err = str(exc) or exc.__class__.__name__
    if repl is not None:
        if repl.load_stack:
            err += f" {repl.load_stack_str()}"      # where: the file/dep being loaded
        if repl.last_val:
            err += f" (last val: {repl.last_val})"   # last value bound before failure
    return err


def run_test(test, test_timeout, env=None):
    """Run a single test in a fresh Candle process. Never raises."""
    safe_name = test.name.replace("/", "_")
    fd, log_path = tempfile.mkstemp(prefix=f"candle-{safe_name}-", suffix=".log")
    logfile = os.fdopen(fd, "w")

    repl = None
    start = None          # set once Candle is booted; marks the start of hol.ml load
    hol_elapsed = 0.0
    test_elapsed = 0.0
    try:
        repl = CandleREPL(logfile=logfile, env=env)

        start = time.perf_counter()
        repl.load("hol.ml", timeout=test_timeout)
        hol_elapsed = time.perf_counter() - start

        for f in test.files:
            repl.load(f, timeout=test_timeout)
        test_elapsed = time.perf_counter() - start - hol_elapsed

        return TestResult(test.name, TestStatus.PASS, hol_elapsed, test_elapsed,
                          log_path=log_path)

    except Exception as e:  # pylint: disable=broad-exception-caught
        # A test must never crash the runner: record any failure and move on.
        # Attribute elapsed time to whichever phase we failed in.
        if start is not None:
            if hol_elapsed:
                test_elapsed = time.perf_counter() - start - hol_elapsed
            else:
                hol_elapsed = time.perf_counter() - start
        status = (TestStatus.TIMEOUT
                  if isinstance(e, pexpect.TIMEOUT) or "Timeout" in str(e)
                  else TestStatus.FAIL)
        return TestResult(test.name, status, hol_elapsed, test_elapsed,
                          error_message=_format_error(repl, e), log_path=log_path)

    finally:
        if repl is not None:
            repl.kill()
        logfile.close()


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
    def print_summary(results, wall):
        print()
        print(f"{'Test':<40} {'Status':>6} {'Time':>9}")
        print("-" * 58)
        for r in results:
            sym = Reporter.STATUS_SYMBOLS[r.status]
            print(f"{r.name:<40} {sym:>6} {f'{r.total:.1f}s':>9}")
        print("-" * 58)

        counts = {}
        for s in TestStatus:
            c = sum(1 for r in results if r.status == s)
            if c:
                counts[s] = c
        parts = [f"{s.value}: {c}" for s, c in counts.items()]

        sum_total = sum(r.total for r in results)
        hol_times = [r.hol_elapsed for r in results if r.hol_elapsed > 0]
        avg_hol = sum(hol_times) / len(hol_times) if hol_times else 0.0

        print(f"Total: {len(results)}  |  " + "  ".join(parts))
        print(f"Sum of per-test time: {sum_total:.1f}s")
        print(f"Wall clock:           {wall:.1f}s")
        print(f"Average hol.ml load:  {avg_hol:.1f}s")

        failures = [r for r in results if r.status in (TestStatus.FAIL, TestStatus.TIMEOUT)]
        if failures:
            print()
            print("FAILURES:")
            for r in failures:
                msg = f"  {r.name}: {r.status.value}"
                if r.error_message:
                    msg += f" — {r.error_message}"
                print(msg)
                print(f"    log: {r.log_path}")


# ---------------------------------------------------------------------------
# Runner
# ---------------------------------------------------------------------------

def _available_memory_mb():
    """Best-effort available RAM in MB (Linux /proc/meminfo), else None."""
    try:
        with open("/proc/meminfo", encoding="utf-8") as f:
            for line in f:
                if line.startswith("MemAvailable:"):
                    return int(line.split()[1]) // 1024
    except (OSError, ValueError, IndexError):
        pass
    return None


def cap_jobs_for_heap(jobs, heap_mb):
    """Cap workers so heap_mb * jobs stays within available RAM.

    Each candle process reserves its full heap, so heap_mb * jobs is the
    memory ceiling. Falls back to the requested jobs if memory is unknown.
    """
    avail = _available_memory_mb()
    if avail is None:
        return jobs
    return max(1, min(jobs, avail // heap_mb))


def run_suite(tests, jobs, test_timeout, env=None):
    total = len(tests)
    print(f"Running {total} test(s) with {jobs} parallel worker(s).")
    print("Logs: /tmp/candle-<test>-*.log\n")

    results = []
    done = 0
    wall_start = time.perf_counter()

    with concurrent.futures.ThreadPoolExecutor(max_workers=jobs) as ex:
        futures = {ex.submit(run_test, t, test_timeout, env): t for t in tests}
        try:
            for fut in concurrent.futures.as_completed(futures):
                r = fut.result()
                done += 1
                sym = Reporter.STATUS_SYMBOLS[r.status]
                print(f"[{done}/{total}] {sym}  {r.name}  ({r.total:.1f}s)")
                if r.status is not TestStatus.PASS:
                    if r.error_message:
                        print(f"          {r.error_message}")
                    print(f"          log: {r.log_path}")
                results.append(r)
        except KeyboardInterrupt:
            print("\nInterrupted — cancelling pending tests; showing results so far.")
            for fut in futures:
                fut.cancel()

    wall = time.perf_counter() - wall_start
    return results, wall


# ---------------------------------------------------------------------------
# CLI
# ---------------------------------------------------------------------------

def main():
    parser = argparse.ArgumentParser(description="Candle parallel regression suite")
    parser.add_argument(
        "--top100", action="store_true",
        help="Run the full Top 100 theorems suite instead of the regression subset",
    )
    parser.add_argument(
        "--test", nargs="+",
        help="Run specific test name(s), e.g. 100/arithmetic (overrides suite selection)",
    )
    parser.add_argument(
        "--list", action="store_true",
        help="List the selected suite and exit",
    )
    parser.add_argument(
        "-j", "--jobs", type=int, default=max(1, (os.cpu_count() or 2) - 1),
        help="Number of parallel workers (default: CPU count - 1)",
    )
    parser.add_argument(
        "--timeout", type=int, default=600,
        help="Timeout in seconds for each #use load, including hol.ml (default: 600)",
    )

    args = parser.parse_args()

    if args.test:
        tests = [BY_NAME.get(name, _t(name)) for name in args.test]
        running_top100 = False
    elif args.top100:
        tests = TOP100
        running_top100 = True
    else:
        tests = REGRESSION
        running_top100 = False

    if args.list:
        for t in tests:
            extra = f"  ({', '.join(t.files)})" if t.files != (f"{t.name}.ml",) else ""
            print(f"  {t.name}{extra}")
        print(f"\n{len(tests)} test(s)")
        return

    # The Top 100 suite gets a larger heap; cap parallelism so the combined
    # per-process heap reservation does not exceed available memory.
    child_env = None
    jobs = args.jobs
    if running_top100:
        child_env = {**os.environ, "CML_HEAP_SIZE": str(TOP100_HEAP_MB)}
        jobs = cap_jobs_for_heap(args.jobs, TOP100_HEAP_MB)
        if jobs < args.jobs:
            print(f"Capping workers {args.jobs} -> {jobs}: {TOP100_HEAP_MB} MB heap "
                  f"x {args.jobs} workers exceeds available RAM.")

    results, wall = run_suite(tests, jobs, args.timeout, child_env)
    Reporter.print_summary(results, wall)

    unexpected = [r for r in results if r.status in (TestStatus.FAIL, TestStatus.TIMEOUT)]
    sys.exit(1 if unexpected else 0)


if __name__ == "__main__":
    main()
