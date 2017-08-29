#!/bin/sh -
set -eu
${CC:-clang} -Wall -Wextra -pedantic -std=c99 -g \
    -o ttf2tga ttf2tga.c \
    $(freetype-config --cflags --libs)
