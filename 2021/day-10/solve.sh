#!/usr/bin/env bash
echo "# build"
./build.sh

echo "# test run"
./a.out example.txt

echo "# run"
./a.out input.txt
