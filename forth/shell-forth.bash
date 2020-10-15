#!/bin/bash
set -u
top=0; a=""; b=""
underflow() { echo "Stack underflow" >&2; exit 2; }
not_empty() { (($top > 0)) || underflow; }
dec_top() { not_empty; top=$(($top - 1)); }
inc_top() { top=$(($top + 1)); }
push_a() { stack[$top]="$a"; inc_top; }
pop_a() { dec_top; a="${stack[$top]}"; }
pop_b() { dec_top; b="${stack[$top]}"; }
pop_ab() { pop_b; pop_a; }
show() { pop_a; printf "%s\n" "$a"; }
sub() { pop_ab; a="$(expr "$a" "-" "$b")"; push_a; }
mul() { pop_ab; a="$(expr "$a" "*" "$b")"; push_a; }
a=10; push_a; a=3; push_a; sub; a=4; push_a; mul; show
