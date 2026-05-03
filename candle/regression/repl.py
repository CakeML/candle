from dataclasses import dataclass, field
import os
import subprocess
import sys
import time
import pexpect
from common import CANDLE_ROOT


class StartFailure(Exception):
    """Starting Candle failed (pre-boot)."""


class BootFailure(Exception):
    """Booting Candle failed (pre-boot)."""


class REPLError(Exception):
    """A command sent to the REPL has failed."""


@dataclass
class Checkpoint:
    """Handle returned by CandleREPL.checkpoint() and consumed by restore().

    All checkpoints from a single CandleREPL instance share `cake_pid`
    (CRIU restores into the original PID), so at most one can be live at
    a time.
    """

    dir: str
    cake_pid: int
    load_stack: list = field(default_factory=list)
    last_val: str | None = None


class CandleREPL:
    def __init__(self):
        # Easier to assume that we are in the candle directory for now.
        os.chdir(CANDLE_ROOT)

        # The Candle (cake) PID. After a restore, self.process.pid is the
        # `sudo criu restore` wrapper, not Candle, so we track this separately.
        # Set on first checkpoint() or restore().
        self._cake_pid = None

        self._logfile = sys.stdout
        self._checkpoints_root = CANDLE_ROOT / "checkpoints"
        self._checkpoint_counter = 0

        self.load_stack = []
        self.last_val = None

        try:
            self.process = pexpect.spawn(
                str(CANDLE_ROOT / "candle.sh"), encoding="utf-8", logfile=self._logfile
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
            index = self.process.expect(
                [
                    r"\n# ",
                    r"\n(ERROR: .+)",
                    pexpect.TIMEOUT,
                    pexpect.EOF,
                ]
            )
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

    def _context_str(self):
        after = f"(after val {self.last_val}) " if self.last_val else ""
        return f"{after}[while loading: {' > '.join(self.load_stack)}]"

    def _check_output(self, timeout=600):
        try:
            index = self.process.expect(
                [
                    r"\n\- Loading (\S+)",
                    r"\nval (\w+) =",
                    r"\n(ERROR: .+)",
                    r"\n(Parsing failed)",
                    r"\n(EXCEPTION: .+)",
                    r"\n\- Finished loading (\S+)",
                    pexpect.TIMEOUT,
                    pexpect.EOF,
                ],
                timeout=timeout,
            )
        except Exception as e:
            raise REPLError from e

        match index:
            case 0:
                dependency = self._get_match(1)
                self.load_stack.append(dependency)
            case 1:
                self.last_val = self._get_match(1)
            case 2 | 3 | 4:
                raise REPLError(f"{self._get_match(1)} {self._context_str()}")
            case 5:
                finished = self._get_match(1)
                expected = self.load_stack.pop()
                assert (
                    finished == expected
                ), f"Expected to finish loading {expected}. Actual: {finished}"
            case 6:
                raise REPLError(f"Timeout waiting for output {self._context_str()}")
            case 7:
                raise REPLError(f"Process exited unexpectedly {self._context_str()}")
            case _:
                assert False, "Unreachable: Did you add a new case in _check_output?"

    def load(self, file, timeout=600):
        self.process.sendline(f'loads "{file}";;')
        self._check_output(timeout=timeout)

        while self.load_stack:
            self._check_output(timeout=timeout)

    def run_gc(self):
        self.process.sendline("Cake.Runtime.fullGC();;")
        self._check_output()

    # kinda sketchy.
    def kill(self):
        self.process.close(force=True)

        # We need to make sure that the Candle process is killed and reaped,
        # as criu tries to restore into the same pid (I think),
        # which will fail if the pid is occupied.
        # My understanding is that after restoring at least Candle is a process
        # group that is not the child of this process. The former motivates the
        # g (group), and the latter is why we have the loop with killpg.
        if self._cake_pid:
            subprocess.run(["pkill", "-9", "-g", str(self._cake_pid)], check=False)
            while True:
                try:
                    os.killpg(self._cake_pid, 0)
                except ProcessLookupError:
                    return
                time.sleep(0.1)

    def checkpoint(self):
        self._checkpoint_counter += 1
        ckpt_dir = self._checkpoints_root / f"{self._checkpoint_counter:04d}"
        os.makedirs(ckpt_dir, exist_ok=True)

        pid = self._cake_pid or self.process.pid

        self.run_gc()

        subprocess.run(
            [
                "sudo",
                "criu",
                "dump",
                "-D",
                str(ckpt_dir),
                "-t",
                str(pid),
                "--shell-job",
                "--leave-running",
            ],
            check=True,
        )

        # Remember the cake PID for subsequent dumps in this lineage; the
        # Python-side process state (self.process) remains usable because
        # --leave-running kept the original alive.
        self._cake_pid = pid

        return Checkpoint(
            dir=str(ckpt_dir),
            cake_pid=pid,
            load_stack=list(self.load_stack),
            last_val=self.last_val,
        )

    def restore(self, checkpoint):
        # CRIU restores into the original PID, so any process holding that PID
        # (the still-running pre-dump process, or a previously restored
        # sibling) must be killed first or restore will fail.
        self.kill()

        self.process = pexpect.spawn(
            "sudo",
            ["criu", "restore", "-D", checkpoint.dir, "--shell-job"],
            encoding="utf-8",
            logfile=self._logfile,
        )
        self._cake_pid = checkpoint.cake_pid
        self.load_stack = list(checkpoint.load_stack)
        self.last_val = checkpoint.last_val
