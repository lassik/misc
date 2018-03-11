#!/bin/sh
${CC:-clang} -Wall -Wextra -g -o loopback-tcp-server loopback-tcp-server.c
