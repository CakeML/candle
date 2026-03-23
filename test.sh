#!/bin/bash
set -euo pipefail

# Set up Candle
./build-instructions.sh

# Run regression suite
python candle-regression.py