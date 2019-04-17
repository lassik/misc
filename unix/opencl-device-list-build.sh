#!/bin/sh
set -eux
${CC:-clang} -Wall -Wextra -std=c99 -g \
	-framework OpenCL \
	-o opencl-device-list opencl-device-list.c
