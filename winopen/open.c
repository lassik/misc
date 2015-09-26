#define _UNICODE
#define UNICODE

#include <windows.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define PROGNAME L"open"

static void diesys(const wchar_t *msg)
{
    static wchar_t errbuf[128];

    FormatMessageW(
        FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS,
        0, GetLastError(), MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
        errbuf, sizeof(errbuf), 0);
    errbuf[sizeof(errbuf)-1] = 0;
    // There's no newline in the format string because WinAPI error
    // messages end in a newline.
    fwprintf(stderr, L"%s: %s: %s", PROGNAME, msg, errbuf);
    exit(1);
}

static void usage(int exitcode, FILE *stream)
{
    fwprintf(stream, L"usage: " PROGNAME L" filename\n");
    exit(exitcode);
}

static void open(const wchar_t *filename)
{
    int ret;

    ret = (int)ShellExecuteW(0, 0, filename, 0, 0, SW_SHOW);
    if (ret <= 32)
        diesys(L"cannot open file");
}

extern int wmain(int argc, wchar_t **argv)
{
    if (argc != 2)
        usage(1, stderr);
    if (!wcscmp(argv[1], L"-h") || !wcscmp(argv[1], L"--help"))
        usage(0, stdout);
    if ((argv[1][0] == '-') || (argv[1][0] == '/'))
        usage(1, stderr);
    open(argv[1]);
    return 0;
}
