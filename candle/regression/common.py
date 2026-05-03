from pathlib import Path

CANDLE_ROOT = Path(__file__).resolve().parent.parent.parent
HOL_PATH = CANDLE_ROOT / "hol.ml"


def resolve(path):
    return (CANDLE_ROOT / path).resolve()


def relative(path):
    return path.relative_to(CANDLE_ROOT)
