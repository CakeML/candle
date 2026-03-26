#!/bin/bash
# Set up "strict mode" for bash
set -euo pipefail

# Change into correct directory - note that Candle must take care to change back
# to script_dir (see build-instructions.sh for more information)
script_dir=$(dirname -- "$(readlink -f -- "${BASH_SOURCE[0]}")")
cd $script_dir/candle/build

# Start Candle
./cake --candle
