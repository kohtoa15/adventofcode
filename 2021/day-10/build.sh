#!/usr/bin/env bash
gcc -g -Wall \
  src/fsio.h src/fsio.c \
  src/chunk.h src/chunk.c \
  src/main.c
