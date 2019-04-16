#! /bin/sh -
set -eu
cd "$(dirname "$0")"
${CC:-clang} -Wall -Wextra -g -O -o chroot-preserves-fd chroot-preserves-fd.c
