#include <stdio.h>

#include "strdupv.h"

extern int main(void)
{
    printf("%s\n", strdup2("", ""));
    printf("%s\n", strdup2("abc", ""));
    printf("%s\n", strdup2("", "def"));
    printf("%s\n", strdup3("hello", " ", "world"));
    printf("%s %s\n", strdup2("foo", "bar"), strdup2("baz", "qux"));
    return(0);
}
