#!/usr/bin/env bash
zig build-exe -target riscv64-linux -femit-bin=out riscv_rt/zig-out/lib/libmincaml.a $1 -O Debug -fno-strip -mcpu=baseline_rv64
./rvlinux -n out
