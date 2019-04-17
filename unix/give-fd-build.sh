#! /bin/sh -
set -eux
cd "$(dirname "$0")"
${CC:-clang} -Wall -Wextra -g -o give-fd give-fd.c
