#!/bin/sh
set -eux
${CC:-clang} -Wall -Wextra -g -o noidentity noidentity.c
