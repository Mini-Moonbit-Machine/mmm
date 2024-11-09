#!/usr/bin/env fish

for file in test/test_src/*.mbt
  rm -f out
  echo "Testing $file"
  moon run src/bin/main.mbt -- --end-stage riscv -notc $file -o out.S
  zig build-lib -target riscv64-linux -femit-bin=out.o out.S -O Debug -fno-strip -mcpu=baseline_rv64
  riscv64-elf-size out.o -Gd | awk 'NR == 2 { print $1 }'
  echo \n
end

for file in test/longer-cases/*.mbt
  rm -f out
  echo "Testing $file"
  moon run src/bin/main.mbt -- --end-stage riscv -notc $file -o out.S
  zig build-lib -target riscv64-linux -femit-bin=out.o out.S -O Debug -fno-strip -mcpu=baseline_rv64
  riscv64-elf-size out.o -Gd | awk 'NR == 2 { print $1 }'
  echo \n
end