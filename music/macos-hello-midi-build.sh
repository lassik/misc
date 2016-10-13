#!/bin/sh -
set -eu
${CC:-clang} -Wall -Wextra -pedantic -std=c99 -g -O \
    -framework Foundation -framework CoreMIDI \
    -o macos-hello-midi macos-hello-midi.c
