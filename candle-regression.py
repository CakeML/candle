import sys
import pexpect
import time


class StartFailure(Exception):
    """Starting Candle failed (pre-boot)."""


class BootFailure(Exception):
    """Booting Candle failed (pre-boot)."""


class LoadFailure(Exception):
    """Loading a file failed."""


class CandleREPL:
    def __init__(self, base):
        self.base = base
        try:
            self.process = pexpect.spawn(os.path.join(self.base, "candle"), encoding='utf-8', logfile=sys.stdout)
        except Exception as e:
            raise StartFailure from e

        try:
            self._check_boot()
        except BootFailure:
            self.kill()
            raise

        self.load_stack = []
        self.last_val = None

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

    def _check_output(self):
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
            ])
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
                pass
            case 7:
                raise LoadFailure("Process exited unexpectedly")
            case _:
                pass

    def load(self, file):
        self.process.sendline(f'#use "{file}";;')

        while self.load_stack:
            self._check_output()

    def kill(self):
        self.process.close(force=True)

    def dump(self):
        pass

    def restore(self):
        pass

candle = CandleREPL()

