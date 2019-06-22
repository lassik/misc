#!/bin/sh
set -eux
${CC:-clang} -Wall -Wextra -ansi -pedantic -Og -g \
	-o version_sort version_sort.c
${CC:-clang} -Wall -Wextra -ansi -pedantic -Og -g \
	-o version_sort_old version_sort_old.c
