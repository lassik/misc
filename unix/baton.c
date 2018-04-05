/* http://catb.org/jargon/html/T/twirling-baton.html */

#include <stdio.h>
#include <unistd.h>

static int baton_next(void)
{
    const char baton[] = "|/-\\";
    static const char *p = 0;

    if(!p || !*p) p = baton;
    return(*p++);
}

int main()
{
    for(;;) {
        printf("\r%c", baton_next());
        fflush(stdout);
        usleep(50000);
    }
    return(0);
}
