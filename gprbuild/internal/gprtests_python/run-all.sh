#!/bin/sh

# Run all supported config and store outputs in ./out/config_name

for conf in `./run-gprbuild-test.py $@ --config-list`; do
   echo $conf
   ./run-gprbuild-test.py $@ --config=$conf --output-dir=./out/$conf
done
