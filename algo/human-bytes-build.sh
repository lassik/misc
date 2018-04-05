#!/bin/sh
set -eu
${CC:-clang} -Wall -Wextra -o human-bytes human-bytes.c
