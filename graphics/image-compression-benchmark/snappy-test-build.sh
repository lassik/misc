#!/bin/sh
set -eux
clang -lsnappy -o snappy-test snappy-test.c
