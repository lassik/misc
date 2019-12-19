#!/bin/sh -
set -eu
gcc -Werror -Wall -Wextra -pedantic -ansi \
    -o anonymous-struct anonymous-struct.c
