#!/usr/bin/env bash

set -o pipefail
set -eu

echo "# build"
./build.sh
echo ""

echo "# run test 1"
./a.out example-1.txt
echo ""

echo "# run test 2"
./a.out example-2.txt
echo ""

echo "# run test 3"
./a.out example-3.txt
echo ""

echo "# run"
./a.out input.txt
echo ""
