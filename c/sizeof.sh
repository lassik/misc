#!/bin/sh -
exec 2>&1
[ $# -gt 0 ] || { echo "usage: $0 compiler flags ..."; exit 1; }
echo "$(uname) $(uname -m) $(uname -p)"
set -eux
$@ -o sizeof sizeof.c
./sizeof
