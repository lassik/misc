#include <sys/types.h>

#include <dirent.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static void diesys(const char *msg)
{
    fprintf(stderr, "%s: %s\n", msg, strerror(errno));
    exit(1);
}

static void do_ent(const char *dirpath, const char *name)
{
    printf("%s/%s\n", dirpath, name);
}

static void do_dir(const char *dirpath)
{
    DIR *handle;
    struct dirent *d;
    int firsterror;

    if (!(handle = opendir(dirpath)))
        goto done;
    for (;;) {
        errno = 0;
        if (!(d = readdir(handle)))
            break;
        if (!strcmp(d->d_name, ".") || !strcmp(d->d_name, ".."))
            continue;
        do_ent(dirpath, d->d_name);
    }
done:
    firsterror = errno;
    if (handle && (closedir(handle) == -1) && !firsterror)
        firsterror = errno;
    if ((errno = firsterror))
        diesys("cannot list directory");
}

extern int main(void)
{
    do_dir(".");
    return 0;
}
