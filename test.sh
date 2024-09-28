#!/usr/bin/env fish

for file in test/test_src/*.mbt
  echo "Testing $file"
  moon run src/bin/main.mbt -- --closure-interpreter $file
end