#!/bin/bash
set -eu -o pipefail
cd "$(dirname "$0")"
echo "Entering directory '$PWD'"
set -x
docker build -t vx-convert .
docker run vx-convert cat vx-scheme-20080617-repo.git.tar.gz \
    >vx-scheme-20080617-repo.git.tar.gz.new
mv -f vx-scheme-20080617-repo.git.tar.gz.new \
    vx-scheme-20080617-repo.git.tar.gz
