#!/usr/bin/env bash
gcc -g -Wall \
  src/fsio.h src/fsio.c \
  src/graph.h src/graph.c \
  src/gpath.h src/gpath.c \
  src/main.c
