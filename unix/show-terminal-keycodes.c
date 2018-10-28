#include <sys/types.h>

#include <ctype.h>
#include <stdio.h>
#include <termios.h>
#include <unistd.h>

static void
dumpstringbyte(int c)
{
    if (isprint(c)) {
        printf("%c", c);
    } else {
        printf("\\x%02x", c);
    }
}

static void
dump(void)
{
    unsigned char bytes[8];
    ssize_t i, n;

    while ((n = read(0, bytes, sizeof(bytes))) > 0) {
        printf("{ ");
        for (i = 0; i < n; i++) {
            printf("%02x ", bytes[i]);
        }
        printf("} \"");
        dumpstringbyte(bytes[0]);
        printf("\"");
        if (n > 1) {
            printf(" \"");
            for (i = 1; i < n; i++) {
                dumpstringbyte(bytes[i]);
            }
            printf("\"");
        }
        printf("\r\n");
        if (bytes[0] == 'q') {
            break;
        }
    }
}

int
main(void)
{
    struct termios origmode;
    struct termios rawmode;

    cfmakeraw(&rawmode);
    tcgetattr(0, &origmode);
    tcsetattr(0, TCSAFLUSH, &rawmode);
    dump();
    tcsetattr(0, TCSAFLUSH, &origmode);
    return 0;
}
