// MacOS             no ampersands, no commas
// BeOS (Haiku)      no ampersands, no commas
// Solaris (OmniOS)  no ampersands, no commas
// Minix             no ampersands, no commas
// Linux (Alpine)    no ampersands, uses commas
// Linux (Debian)    no ampersands, uses commas
// FreeBSD           uses ampersand, no commas
// OpenBSD           uses ampersand, no commas
// NetBSD            uses ampersand, no commas
// DragonFly BSD     uses ampersand, no commas

#include <sys/types.h>

#include <ctype.h>
#include <pwd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

char *
parse_full_name(const char *user, const char *gecos)
{
    const char *p;
    char *ans;
    size_t before_amp, user_len, after_amp;

    before_amp = user_len = after_amp = 0;
    p = gecos;
    while (*p && (*p != ',') && (*p != '&')) {
        p++; before_amp++;
    }
    if (*p == '&') {
        p++;
        while (*p && (*p != ',')) {
            p++; after_amp++;
        }
        user_len = strlen(user);
    }
    if (!(ans = calloc(1, before_amp + user_len + after_amp + 1))) {
        exit(1);
    }
    memcpy(ans, gecos, before_amp);
    if (user_len) {
        memcpy(ans + before_amp, user, user_len);
        ans[before_amp] = toupper(ans[before_amp]);
    }
    if (after_amp) {
        memcpy(ans + before_amp + user_len, gecos + before_amp + 1, after_amp);
    }
    return ans;
}

int
main(void)
{
    struct passwd *pw;
    char *full;

    while ((pw = getpwent())) {
        full = parse_full_name(pw->pw_name, pw->pw_gecos);
        if (strlen(full)) {
            printf("%s\n", full);
        }
        free(full);
    }
    return 0;
}
