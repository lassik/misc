#!/bin/sh
cc -Wall -pedantic -std=c99 -Og -g \
   -o x-hide-mouse x-hide-mouse.c \
   -I /usr/X11R6/include -L /usr/X11R6/lib -l X11
