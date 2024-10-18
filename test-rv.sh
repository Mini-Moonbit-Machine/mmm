#!/usr/bin/env fish
for file in test/test_src/*.mbt
  if [ $file = "test/test_src/bf.mbt" ]
    continue
  end
  rm -f out
  echo "Testing $file"
  moon run -g src/bin/main.mbt -- --end-stage riscv $file -o out.S
  zig build-exe -target riscv64-linux -femit-bin=out riscv_rt/zig-out/lib/libmincaml.a out.S -O Debug -fno-strip -mcpu=baseline_rv64
  ./rvlinux -n out
  echo \n
  riscv64-elf-size out
  echo \n
end