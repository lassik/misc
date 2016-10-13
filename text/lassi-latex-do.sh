#!/bin/sh
# this part of the script is written in shell instead of
# ruby because shell is more convenient for these tasks.
f="$1"
exec 2>&1
exec 1>"$f.runlog"
echo -n "run started "
date "+%Y-%m-%d %H:%M%z"
echo
latex "$f.tex"
dvipdf "$f.dvi"
daemon xpdf "$f.pdf"
