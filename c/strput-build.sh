#!/bin/sh -
set -eu
cc="${CC:-clang} -Wall -Wextra -std=c99 -g -O"
$cc -o strput-test strput-test.c strput.c
$cc -o strput-bench strput-bench.c strput.c
