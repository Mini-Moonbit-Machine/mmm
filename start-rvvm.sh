#!/usr/bin/env bash
pushd ~/Projects/rvvm/build
./rvvm fw_payload.bin -i archriscv.img -m 2G -portfwd tcp/127.0.0.1:2022=22
popd