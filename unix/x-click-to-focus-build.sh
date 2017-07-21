#!/bin/sh
gcc -Wall -pedantic -std=c99 -g -o x-click-to-focus x-click-to-focus.c -I /usr/X11R6/include -L /usr/X11R6/lib -l X11
