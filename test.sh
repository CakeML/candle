#!/bin/bash
set -euo pipefail

# Set up Candle
./build-instructions.sh

# Check proof replay without checkpointing
python3 candle/pft/test_replay.py

# Run regression suite
python candle/regression.py
