#!/bin/sh
set -eux
${CC:-clang} -Wall -Wextra -g -o tcp-connections-darwin tcp-connections-darwin.c
