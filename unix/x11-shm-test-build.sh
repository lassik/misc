#! /bin/sh
set -eu
${CC:-clang} -Wall -Wextra -o x11-shm-test x11-shm-test.c \
             -I /usr/X11/include -L /usr/X11/lib -l X11
