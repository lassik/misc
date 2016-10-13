// Get pathname of running executable on FreeBSD.
//
// TODO: This only gets the basename, and long names are truncated.

#include <sys/types.h>
#include <sys/user.h>
#include <libutil.h>

#include <unistd.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static char *getmyname(void)
{
    struct kinfo_proc *proc;
    char *name;

    if (!(proc = kinfo_getproc(getpid())))
        return 0;
    name = strdup(proc->ki_comm);
    free(proc);
    return name;
}

extern int main(void)
{
    char *name;

    name = getmyname();
    printf("%s\n", name);
    free(name);
    return 0;
}
