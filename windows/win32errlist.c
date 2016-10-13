// Dump the full range of Win32 error codes and their messages to
// stdout. Incredibly, I couldn't find such a list on the web.
//
// System locale should be US English.

#undef _UNICODE
#undef UNICODE

#include <windows.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct error {
    unsigned int errcode;
    const char *errname;
};

static struct error errors[] = {
#include "win32errliststructs.h"
};

static const size_t nerrors = sizeof(errors) / sizeof(*errors);
static char errbuf[256];

static int compare_errors(const void *cva, const void *cvb)
{
    const struct error *a = (const struct error *)cva;
    const struct error *b = (const struct error *)cvb;

    if (a->errcode < b->errcode) return -1;
    if (a->errcode > b->errcode) return 1;
    return 0;
}

// A few error messages are multi-line, having embedded newlines.
// Most (all?) error messages have a trailing newline (CR or CR-LF).
static void convert_all_whitespace_to_spaces(void)
{
    static const char whitespace[] = "\r\n\t ";
    char *p;

    for (p = errbuf; *p; p++) {
        if (strchr(whitespace, *p)) {
            *p = ' ';
        }
    }
}

static void remove_trailing_spaces(void)
{
    char *p;

    p = strchr(errbuf, 0);
    while ((p > errbuf) && (p[-1] == ' ')) {
        p--;
    }
    *p = 0;
}

extern int main(void)
{
    const struct error *error;

    qsort(errors, nerrors, sizeof(*errors), compare_errors);
    for (error=errors; error<errors+nerrors; error++) {
        if (FormatMessageA(
                FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS,
                0, error->errcode, MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
                errbuf, sizeof(errbuf), 0)) {
            convert_all_whitespace_to_spaces();
            remove_trailing_spaces();
            printf("0x%04lx (% 5lu) [%s] %s\n",
                   error->errcode, error->errcode,
                   error->errname, errbuf);
        }
    }
    return 0;
}
