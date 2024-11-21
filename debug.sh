#!/usr/bin/env bash
rm -f out
moon run -g src/bin/main.mbt -- $1 -o $1.S
zig build-exe -target riscv64-linux -femit-bin=out riscv_rt/zig-out/lib/libmincaml.a $1.S -O Debug -fno-strip -mcpu=baseline_rv64
./rvlinux -n out
