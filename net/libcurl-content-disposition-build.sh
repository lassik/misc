#!/bin/sh -
${CC:-clang} -Wall -Wextra -pedantic -std=c99 -g -O \
             $(pkg-config --cflags --libs libcurl) \
             -o libcurl-content-disposition libcurl-content-disposition.c
