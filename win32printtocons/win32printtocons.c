// usage: win32printtocons blue foo red bar

#undef _UNICODE
#undef UNICODE

#include <windows.h>

#include <stdio.h>
#include <string.h>

#define WHITE_ON_BLACK (FOREGROUND_RED | FOREGROUND_GREEN | FOREGROUND_BLUE)

static HANDLE cons;

static void check(long rv)
{
    if (rv <= 0) {
        fprintf(stderr, "error\n");
        exit(1);
    }
}

static void reset(void)
{
    check(SetConsoleTextAttribute(cons, WHITE_ON_BLACK));
}

static void evaluate(const char *arg)
{
    if (!strcmp(arg, "reset")) {
        reset();
    } else if (!strcmp(arg, "red")) {
        check(SetConsoleTextAttribute(cons, FOREGROUND_RED));
    } else if (!strcmp(arg, "green")) {
        check(SetConsoleTextAttribute(cons, FOREGROUND_GREEN));
    } else if (!strcmp(arg, "blue")) {
        check(SetConsoleTextAttribute(cons, FOREGROUND_BLUE));
    } else {
        check(printf("%s", arg));
        check(fflush(stdout) == 0);
        // Flushing is mandatory. Otherwise text output gets delayed
        // (buffered) and when it's finally output, the wrong color is
        // likely active. This is easy to demonstrate by commenting
        // out the fflush() call.
    }
}

int main(int argc, char **argv)
{
    int i;

    check(0 != (cons = GetStdHandle(STD_OUTPUT_HANDLE)));
    for (i=1; i<argc; i++) {
        evaluate(argv[i]);
    }
    reset();
    return 0;
}
