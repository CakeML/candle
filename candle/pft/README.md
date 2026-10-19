# Proof replay

The reader replays binary proof traces emitted for Candle by HOL's PFT
converter. It accepts the `PFT\0` magic bytes, unsigned integer format version
`1`, and ruleset `candle`. It does not accept JSON Lines or the historical
string version `0.1.0`.

Build Candle using `build-instructions.sh`, start it from the repository root
with `./candle.sh`, and load the reader:

```ocaml
#use "candle/pft/replay.ml";;
replay_all ["traces/candle.candle.pft.bin";
            "traces/bool.candle.pft.bin";
            "traces/example.candle.pft.bin"];;
```

Paths are relative to the repository root, or absolute. Files run in list
order, retaining named `SAVE` theorems for subsequent `LOAD` commands. Loading
the reader itself does not replay any files. `replay path` handles one file;
`load_th name` retrieves a saved theorem.

The format has four independent ID namespaces: types, terms, theorems, and
compute contexts. `COMPUTE_INIT` (`0x40`) stores a list of characteristic
equations under a context ID; `COMPUTE` (`0x41`) refers to that context and
supplies its term and code equations. `NEW_TYPE_DEFINITION` (`0x31`) assigns
the abstraction theorem to its result ID and the representation theorem to
the following ID.

The header is checked before any replay kernel calls. Before allocating object
arrays, the reader validates the final `0xFF` footer, its four unsigned counts,
and its trailing little-endian uint16 length (excluding the length field).
Because Candle's `Text_io` interface has no seek operation, this preflight
scans the file once while retaining at most 65,537 bytes; it creates no sidecar
file and invokes no external commands. Replay then requires a matching terminal
footer with no following data. Rejection during replay does not undo earlier
kernel declarations or saved theorems.

Deletion commands are consumed without releasing array entries. Namespace
counts must accommodate all IDs used by the trace, including both results of
a type definition.

## Tests

After building the runtime, run from the repository root:

```sh
python3 candle/pft/test_replay.py
```

On hosts that need an x86-64 emulator:

```sh
python3 candle/pft/test_replay.py --runner /path/to/qemu-x86_64
```

The tests cover supported and rejected headers, rejection before replay kernel
calls, footer validation, paths containing shell punctuation, a trace larger
than the footer buffer, explicit compute-context records, both type-definition
results, and cross-file theorem loading. They create no axioms. Actual compute
evaluation requires the characteristic-equation theories and should also be
checked with converted HOL traces.
