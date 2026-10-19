#!/usr/bin/env python3
"""Exercise the PFT reader in a built Candle runtime, without checkpointing."""

import argparse
import json
import os
from pathlib import Path
import subprocess
import tempfile


def varint(value: int) -> bytes:
    result = bytearray()
    while value >= 128:
        result.append((value & 127) | 128)
        value >>= 7
    result.append(value)
    return bytes(result)


def string(value: str) -> bytes:
    encoded = value.encode()
    return varint(len(encoded)) + encoded


def command(opcode: int, *arguments: int | str) -> bytes:
    return bytes([opcode]) + b"".join(
        string(value) if isinstance(value, str) else varint(value)
        for value in arguments
    )


def footer(counts: tuple[int, int, int, int]) -> bytes:
    encoded = command(0xFF, *counts)
    return encoded + len(encoded).to_bytes(2, "little")


def trace(body: bytes, counts: tuple[int, int, int, int]) -> bytes:
    return b"PFT\0\1" + string("candle") + body + footer(counts)


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--runner", help="Optional emulator executable, e.g. qemu-x86_64")
    args = parser.parse_args()
    root = Path(__file__).resolve().parents[2]
    runtime = ([args.runner] if args.runner else []) + ["./cake", "--candle"]
    with tempfile.TemporaryDirectory(prefix="pft test ' ", dir=root / "candle/build") as tmp:
        directory = Path(tmp)
        cases: list[tuple[str, str | None, bool]] = []

        def case(name: str, data: bytes, error: str | None = None, *, early: bool = False) -> None:
            (directory / name).write_bytes(data)
            cases.append((name, error, early))

        # The mutation command must never run when a header is rejected.
        mutation = command(0x08, "pft_rejected_type", 0)
        ending = footer((0, 0, 0, 0))
        case("v0", b"PFT\0\0" + string("candle") + mutation + ending,
             "expect_version: unsupported version 0; expected 1", early=True)
        case("v2", b"PFT\0\2" + string("candle") + mutation + ending,
             "expect_version: unsupported version 2; expected 1", early=True)
        case("semver", b"PFT\0" + string("0.1.0") + string("candle") + mutation + ending,
             "expect_version: unsupported version 5; expected 1", early=True)
        case("ruleset", b"PFT\0\1" + string("hol4") + mutation + ending,
             "unsupported ruleset: hol4", early=True)
        # No footer at all: header validation still takes precedence.
        case("header-first", b"PFT\0\2", "expect_version: unsupported version 2; expected 1", early=True)
        case("bad-magic", b"QFT\0\1" + string("candle") + ending,
             "expect_char: expected 80, got 81", early=True)
        case("json", b'{"cmd":"PFT","version":1,"ruleset":"candle"}\n',
             "expect_char: expected 80, got 123", early=True)
        case("empty", trace(b"", (0, 0, 0, 0)))
        case("bad-length", trace(b"", (0, 0, 0, 0))[:-2] + b"\4\0",
             "process_footer: invalid footer length", early=True)
        case("bad-opcode", b"PFT\0\1" + string("candle") + b"\xfe\0\0\0\0\5\0",
             "process_footer: bad footer opcode", early=True)
        case("bad-counts", b"PFT\0\1" + string("candle") + b"\xff\0\0\0\0\0\6\0",
             "process_footer: footer length does not match counts", early=True)
        case("truncated-count", b"PFT\0\1" + string("candle") + b"\xff\0\0\0\x80\5\0",
             "decode_uleb128: EOF", early=True)
        case("data-after-footer", trace(b"", (0, 0, 0, 0)) + ending,
             "command_loop: data after footer")
        case("inconsistent-footer", b"PFT\0\1" + string("candle") + footer((1, 0, 0, 0)) + ending,
             "command_loop: inconsistent footer counts")
        case("footer-as-argument", trace(b"\xe0", (0, 0, 0, 0)),
             "command_loop: unsupported command: 0")
        # Exercise wraparound of the bounded tail buffer and a multi-byte count.
        case("large", trace(command(0xE0, 0) * 33000, (128, 0, 0, 0)))
        body = b"".join([
            command(0x01, 0, "A"), command(0x03, 0, "x", 0),
            command(0x06, 1, 0, 0), command(0x10, 0, 1),
            command(0x40, 0, 1, 0), command(0x40, 1, 0),
            command(0x31, 1, 0, "pft_subset", "pft_abs", "pft_rep"),
            command(0x50, "pft_absthm", 1), command(0x50, "pft_repthm", 2),
        ])
        case("type-definition", trace(body, (1, 2, 3, 2)))
        case("load", trace(command(0x51, 0, "pft_absthm") + command(0x50, "pft_loaded", 0),
                           (0, 0, 1, 0)))
        checks = []
        for name, error, early in cases:
            path = str(directory / name)
            # JSON quoting also supplies valid OCaml string literals for these paths.
            if name == "load":
                checks.append(f'replay_all [{json.dumps(path)}]')
                continue
            checks.append(f'check {json.dumps(path)} {"None" if error is None else "(Some " + json.dumps(error) + ")"} {str(early).lower()}')
        source = '''needs "candle/pft/pft.ml";;
let replay_kernel_calls = ref 0;;
let mk_var p = replay_kernel_calls := !replay_kernel_calls + 1; Kernel.mk_var p;;
let REFL tm = replay_kernel_calls := !replay_kernel_calls + 1; Kernel.REFL tm;;
#use "candle/pft/replay.ml";;
let check path error early =
  let calls = !replay_kernel_calls in
  let before = (types (), constants (), axioms ()) in
  let result = try replay path; None with Failure msg -> Some msg in
  if result <> error then failwith ("unexpected replay result: " ^ path);
  if early && (!replay_kernel_calls <> calls ||
               (types (), constants (), axioms ()) <> before) then
    failwith ("kernel called before rejection: " ^ path);;
let _ =
'''
        source += ";\n".join(checks) + ";\n"
        source += '''let identity = Kernel.mk_abs (Kernel.mk_var ("x", aty), Kernel.mk_var ("x", aty)) in
let ty = mk_type ("pft_subset", [aty]) in
let a = Kernel.mk_var ("a", ty) in
let r = Kernel.mk_var ("r", type_of identity) in
let abs = mk_const ("pft_abs", []) in
let rep = mk_const ("pft_rep", []) in
let expected_abs = mk_eq (mk_comb (abs, mk_comb (rep, a)), a) in
let expected_rep = mk_eq (mk_eq (identity, r), mk_eq (mk_comb (rep, mk_comb (abs, r)), r)) in
if hyp (load_th "pft_absthm") <> [] ||
   not (aconv (concl (load_th "pft_absthm")) expected_abs) then failwith "bad first type theorem";
if hyp (load_th "pft_repthm") <> [] ||
   not (aconv (concl (load_th "pft_repthm")) expected_rep) then failwith "bad second type theorem";
if dest_thm (load_th "pft_loaded") <> dest_thm (load_th "pft_absthm") then failwith "bad cross-file LOAD";
print_endline "PFT_TESTS_PASSED";;
'''
        result = subprocess.run(runtime, cwd=root / "candle/build", input=source,
                                text=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
                                env={**os.environ, "CML_HEAP_SIZE": "8000"}, check=False)
        print(result.stdout)
        if result.returncode != 0 or "PFT_TESTS_PASSED\n" not in result.stdout:
            raise SystemExit("PFT reader regression failed")
        print(f"Passed {len(cases)} replay cases and both type-definition theorem checks")


if __name__ == "__main__":
    main()
