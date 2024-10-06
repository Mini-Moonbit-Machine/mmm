#!/usr/bin/env sh

echo "Generating src/lower/riscv/generated/generated.mbt" 
islec src/core/prelude.isle src/core/core.isle src/riscv/riscv.isle src/lower/riscv/lower.isle > src/lower/riscv/generated/generated.mbt
echo "Done"

