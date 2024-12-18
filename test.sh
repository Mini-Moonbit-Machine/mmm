#!/usr/bin/env fish

for file in test/test_src/*.mbt
  echo "Testing $file"
  moon run src/bin/main.mbt -- --end-stage riscv $file --use-xthead
  echo \n
end

for file in test/longer-cases/*.mbt
  echo "Testing $file"
  moon run src/bin/main.mbt -- --end-stage riscv $file --use-xthead
  echo \n
end

for file in test/more-tests/*.mbt
  echo "Testing $file"
  moon run src/bin/main.mbt -- --end-stage riscv $file --use-xthead
  echo \n
end