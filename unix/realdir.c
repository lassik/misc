#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static char *
realdir(const char *filename)
{
    char *path;
    char *p;

    if (!(path = realpath(filename, NULL)))
        return (NULL);
    if ((p = strrchr(path, '/')) > path)
        *p = '\0';
    else
        p[1] = 0;
    return path;
}

int
main(int argc, char **argv)
{
    char *dir;

    if (argc != 2)
        return 2;
    if (!(dir = realdir(argv[1])))
        return 1;
    printf("%s\n", dir);
    free(dir);
    return 0;
}
