#!/bin/sh
exec 2>&1
set -eux
gunzip /svn/vx-scheme-20080617-repo.svndump.gz
svnadmin create /svn/vx-scheme-20080617-repo
svnadmin load /svn/vx-scheme-20080617-repo \
    </svn/vx-scheme-20080617-repo.svndump
git svn clone --stdlayout \
    file:///svn/vx-scheme-20080617-repo \
    /git/vx-scheme-20080617-repo
git --git-dir=/git/vx-scheme-20080617-repo/.git \
    tag v0.7 refs/remotes/origin/tags/v0.7
git clone --bare \
    /git/vx-scheme-20080617-repo \
    /git/vx-scheme-20080617-repo.git
