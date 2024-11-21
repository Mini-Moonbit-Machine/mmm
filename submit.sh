#!/usr/bin/env bash
set -ex
git archive -o "submit-$(git rev-parse --short HEAD)-$(python conf.py).zip" $1 ./src ./moon.mod.json ./mmm.json