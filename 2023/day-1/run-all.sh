#!/bin/bash
set -eu
gcc -g -Wall main.c -o day1
echo "test1"
./day1 < test1.txt
echo "test2"
./day1 < test2.txt
echo "input"
./day1 < input.txt
