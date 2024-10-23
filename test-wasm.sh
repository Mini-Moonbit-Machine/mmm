#!/usr/bin/env bash

for file in test/test_src/*.mbt
do
  echo "Testing $file"
  moon run src/bin/main.mbt -q -g -- --end-stage wasm $file -o $file.wat &&
  wasm-tools parse $file.wat -o $file.wasm &&
  wasm-tools validate $file.wasm &&
  node wasm_rt/runtime.mjs $file.wasm
  
  ret=$?
  if [ $ret -eq 0 ]; then
    echo
    echo "Test $file succeeded"
    rm -rf $file.wat $file.wasm
  else
    echo "Test $file failed"
  fi
done