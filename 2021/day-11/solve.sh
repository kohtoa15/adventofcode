#!/usr/bin/env bash
echo "# build"
./build.sh

echo "# run test"
./a.out example.txt 10

echo "# run"
./a.out input.txt 100
