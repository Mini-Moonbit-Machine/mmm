#!/usr/bin/env bash
set -ex
if [ "$#" -eq 0 ]; then
  stash=$(git stash create)
  filename="submit-$(git rev-parse --short $stash)-$(python conf.py).zip"
  git archive -o $filename $stash ./src ./moon.mod.json ./mmm.json
fi
if [ "$#" -eq 1 ]; then
  filename="submit-$(git rev-parse --short $1)-$(python conf.py).zip"
  git archive -o $filename $1 ./src ./moon.mod.json ./mmm.json
fi
