#!/usr/bin/env bash

for file in configure/*.json
do
  echo "Make submission file with conf $file"
  cp -f $file mmm.json
  ./submit.sh
done

cp -f configure/default.json mmm.json
