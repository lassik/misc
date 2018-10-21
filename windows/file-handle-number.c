#define _UNICODE
#define UNICODE

#include <windows.h>

#include <stdio.h>
#include <string.h>

int
main(void)
{
    SECURITY_ATTRIBUTES sa;
    HANDLE hRead, hWrite;

    memset(&sa, 0, sizeof(sa));
    sa.nLength = sizeof(sa);
    sa.bInheritHandle = TRUE;
    if (!CreatePipe(&hRead, &hWrite, &sa, 0)) {
        printf("error: CreatePipe\n");
    }
    printf("read  file handle: %lu\n", (unsigned long)hRead);
    printf("write file handle: %lu\n", (unsigned long)hWrite);
    return 0;
}
