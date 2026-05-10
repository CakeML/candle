#!/bin/bash
# Set up "strict mode" for bash
set -euo pipefail

# Create (if needed) + Change into the build directory
mkdir -p candle/build
cd candle/build

# Get the 64-bit CakeML compiler from here:
curl -OL https://cakeml.org/regression/artefacts/3286/cake-x64-64.tar.gz
tar xvzf cake-x64-64.tar.gz --strip-components=1

# By default, the CakeML compiler reserves a few kilobytes for constants and
# code produced by the dynamic compiler. Using Candle requires setting these
# to some megabytes (or hundreds of megabytes for some of the more heavier
# files in HOL Light, such as make_complex.ml).
patch cake.S ../cake.S.patch

# Patching in useful FFI calls
patch basis_ffi.c ../basis_ffi.c.patch

# Build the compiler binary
make

# Create the types.txt file necessary for candle_insulate.py
./cake --types < /dev/null > types.txt 2>&1

# Generate candle_insulate.ml
python ../insulate.py types.txt insulate.ml

#  The working directory of the binary will be CANDLE_ROOT/candle/build,
#  so it needs to change directory to CANDLE_ROOT after booting..
#
#  Q: Why do we not start the cake binary from CANDLE_ROOT?
#  A: The cake binary looks for config_enc_str.txt and candle_boot.ml relative
#     to the current working directoy. I find it neater if these files are not
#     copied to CANDLE_ROOT, but stay tucked away in the build folder.
#
cat ../chdir_to_root.ml >> candle_boot.ml

# You can now run Candle by writing:
#   $ ./candle.sh
# (without the $) at your prompt. Load the HOL Light sources by writing:
#   > #use "hol.ml";;
# (without > and with double semicolons) in the REPL.
#
