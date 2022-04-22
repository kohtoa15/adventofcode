#!/usr/bin/env bash
echo "----- BUILD -----"
./build.sh

echo ""
echo "----- RUN -----"
./a.out input.txt 100 100
