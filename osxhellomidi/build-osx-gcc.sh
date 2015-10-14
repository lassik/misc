#!/bin/sh -
gcc -Wall -Wextra -pedantic -std=c99 -g -O \
    -framework Foundation -framework CoreMIDI \
    -o hellomidi hellomidi.c
