#!/usr/bin/env bash
rm -f out
moon run -g src/bin/main.mbt -- --end-stage wasm $1 -o $1.wat