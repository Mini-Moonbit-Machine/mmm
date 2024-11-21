#!/usr/bin/env fish

for file in test/test_src/*.mbt
  echo "Testing $file"
  rm -f out
  rm -f out.S
  moon run src/bin/main.mbt -- --end-stage riscv -notc $file -o out.S
  zig build-exe -target riscv64-linux -femit-bin=out riscv_rt/zig-out/lib/libmincaml.a out.S -O Debug -fno-strip -mcpu=baseline_rv64
  ./rvlinux -n out
  echo \n
end