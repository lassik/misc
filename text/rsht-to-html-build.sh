#!/bin/sh -
set -eux
${CC:-clang} -Wall -Wextra -ansi -g -o rsht-to-html rsht-to-html.c
