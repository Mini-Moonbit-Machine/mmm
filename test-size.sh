#!/usr/bin/env fish

function work
  set file $argv[1]
  echo "Testing $file"
  moon run src/bin/main.mbt -- --end-stage riscv -notc $file -o out.S
  zig build-lib -target riscv64-linux -femit-bin=out.o out.S -O Debug -fno-strip -mcpu=baseline_rv64
  riscv64-elf-size out.o -Gd | awk 'NR == 2 { print $1 }'
  echo \n
end

if test -n "$argv[1]"
  echo "Testing single file $argv[1]"
  rm -f out.o
  work $argv[1]
  exit
end

for file in test/test_src/*.mbt
  rm -f out.o
  work $file
end

for file in test/longer-cases/*.mbt
  rm -f out.o
  work $file
end