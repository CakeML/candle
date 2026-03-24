#!/bin/bash
set -euo pipefail

# Get the 64-bit CakeML compiler from here:
#curl -OL https://cakeml.org/regression/artefacts/3149/cake-x64-64.tar.gz
tar xvzf cake-x64-64.tar.gz

# By default, the CakeML compiler reserves a few kilobytes for constants and
# code produced by the dynamic compiler. Using Candle requires setting these
# to some megabytes:
patch cake-x64-64/cake.S cake.S.patch

# Build the compiler binary
cd cake-x64-64 && make && cd ..

# Copy the compiler binary, the exported compiler state and candle_boot.ml into
# this directory:
cp cake-x64-64/cake cake-x64-64/config_enc_str.txt cake-x64-64/candle_boot.ml .

# Create the types.txt file necessary for candle_insulate.py
./cake --types < /dev/null

# Generate candle_insulate.ml
python candle_insulate.py types.txt candle_insulate.ml

# You can now run Candle by writing:
#   $ ./cake --candle
# or:
#   $ ./candle
# (without the $) at your prompt. Load the HOL Light sources by writing:
#   > #use "hol.ml";;
# (without > and with double semicolons) in the REPL.
#
