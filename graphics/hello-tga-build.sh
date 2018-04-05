#!/bin/sh -
set -eu
${CC:-clang} -Wall -Wextra -o hello-tga hello-tga.c
