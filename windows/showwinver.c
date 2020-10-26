#include <stdio.h>
#include <stdlib.h>
#include <windows.h>

int
main(void)
{
    OSVERSIONINFOEX ver;
    unsigned int exflag;

    ver.dwOSVersionInfoSize = sizeof(OSVERSIONINFOEX);
    if (GetVersionExA((OSVERSIONINFO *)&ver)) {
        exflag = 1;
    } else {
        ver.dwOSVersionInfoSize = sizeof(OSVERSIONINFO);
        if (GetVersionExA((OSVERSIONINFO *)&ver)) {
            exflag = 0;
        } else {
            printf("GetVersionEx() returned false.\r\n");
            return EXIT_FAILURE;
        }
    }
    printf("%s structure:\r\n", (exflag ? "OSVERSIONINFOEX" : "OSVERSIONINFO"));
    printf("dwMajorVersion    : %d\r\n", ver.dwMajorVersion);
    printf("dwMinorVersion    : %d\r\n", ver.dwMinorVersion);
    printf("dwBuildNumber     : %d\r\n", ver.dwBuildNumber);
    printf("dwPlatformId      : %d\r\n", ver.dwPlatformId);
    printf("szCSDVersion      : %s\r\n", ver.szCSDVersion);
    if (exflag) {
        printf("wServicePackMajor : %d\r\n", ver.wServicePackMajor);
        printf("wServicePackMinor : %d\r\n", ver.wServicePackMinor);
        printf("wSuiteMask        : %d\r\n", ver.wSuiteMask);
        printf("wProductType      : %d\r\n", ver.wProductType);
        printf("wReserved         : %d\r\n", ver.wReserved);
    }
    return EXIT_SUCCESS;
}
