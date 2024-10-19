#!/usr/bin/env fish

for file in test/test_src/*.mbt
  echo "Testing $file"
  moon run src/bin/main.mbt -- --js -notc $file -o out.mjs
  node js_rt/runtime.mjs out.mjs
  echo \n
end
