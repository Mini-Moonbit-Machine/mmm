#!/usr/bin/env fish
for file in test/test_src/*.mbt
  rm out
  if [ $file = "test/test_src/inprod-rec.mbt" ]
    continue
  end
  if [ $file = "test/test_src/inprod.mbt" ]
    continue
  end
  echo "Testing $file"
  moon run -g src/bin/main.mbt -- --end-stage riscv $file > $file.S
  zig build-exe -target riscv64-linux -femit-bin=out riscv_rt/zig-out/lib/libmincaml.a $file.S -O Debug -fno-strip -mcpu=baseline_rv64
  ./rvlinux -n out
  echo \n
end