#! /bin/sh -
set -eu
cd "$(dirname "$0")"
${CC:-clang} -Wall -Wextra -g  -o who-sent-signal who-sent-signal.c
