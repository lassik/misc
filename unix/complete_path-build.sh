#!/bin/sh -
set -eu
${CC:-clang} --shared -o libcomplete_path.so complete_path.c
${CC:-clang} -lreadline -o complete_path_test complete_path_test.c -L. -lcomplete_path
