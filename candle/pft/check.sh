#!/usr/bin/env bash
# Replays HOL4's proof-trace regression fixtures in Candle and checks their
# exact sequents (check.ml). First build Candle (./build-instructions.sh) and,
# in HOL4, run src/postkernel/prooftrace/tests/run.sh all with a tracing HOL
# and the fixture directory given here. HOL is the HOL4 checkout whose
# converter is under test (PFTCLI.uo built); TRACING is the tracing HOL4 build
# whose theory traces are converted as prerequisites.
set -euo pipefail

if [ "$#" -ne 3 ]; then
  echo "usage: candle/pft/check.sh /path/to/HOL /path/to/tracing /fixtures" >&2
  exit 2
fi
hol=$(cd -- "$1" && pwd)
tracing=$(cd -- "$2" && pwd)
fixtures=$(cd -- "$3" && pwd)
# The path goes into an ML string, a sed replacement and replay's shell command.
case "$fixtures" in
  *[!A-Za-z0-9._/+-]*)
    echo "fixture path has a character replay can't pass on: $fixtures" >&2
    exit 2 ;;
esac
pft_dir=$(cd -- "$(dirname -- "$0")" && pwd)
pft="$hol/src/postkernel/prooftrace/pft.sh"
export PFT_HOL="${PFT_HOL:-$tracing/bin/hol}"

"$pft" preamble --output "$fixtures/preamble.pft"
for theory in bool/bool \
  marker/marker num/theories/num metis/normalForms combin/combin \
  simp/src/boolSimpsContext HolSat/sat refute/canonContext \
  metis/normalFormsContext metis/folToolsContext metis/folMappingContext \
  meson/src/mesonLibContext meson/src/Canon_Port_Context \
  IndDef/InductiveDefinitionContext relation/relation \
  num/theories/prim_rec quotient/src/quotient coretypes/pair \
  num/theories/arithmetic coretypes/sum coretypes/one coretypes/option \
  num/theories/While num/theories/numeral num/theories/cv
do
  "$pft" emit --ruleset candle \
    --trace "$tracing/src/${theory%/*}/.hol/objs/${theory##*/}Theory.tr.gz" \
    --output "$fixtures/${theory##*/}.candle.pft"
done
"$pft" compute-preamble --output "$fixtures/compute-preamble.pft"

# Candle starts in candle/build and moves to the repository root, so the
# footer extractor is found from there; replay prefixes trace names with here.
log="$fixtures/candle.log"
cd "$pft_dir/../build"
{ sed -e "s|^let here = \"candle/pft/\";;|let here = \"$fixtures/\";;|" \
      -e 's|^let extract_footer_path = .*|let extract_footer_path = "candle/pft/extract_footer";;|' \
      -e '/^let _ = replay_all \[/d' "$pft_dir/replay.ml"
  cat "$pft_dir/check.ml"
} | ./cake --candle > "$log" 2>&1
# Candle can exit zero after an exception, so check the log itself.
if ! grep -q 'ALL_CANDLE_PASS$' "$log" || grep -q EXCEPTION "$log"; then
  echo "Candle check failed; see $log" >&2
  exit 1
fi
echo "$(grep -c 'PFT_EXACT ' "$log") exact sequents checked; log: $log"
