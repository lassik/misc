// Mass-rename files in the current directory by editing the list of
// filenames in your text editor of choice ($EDITOR). Confirms whether
// to rename each file whose name you changed.
//
// The same can be accomplished using wdired which ships with Emacs.

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/wait.h>

#include <dirent.h>
#include <err.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

static char *xstrdup(char const *str) {
    size_t size;
    char *copy;
    size = strlen(str) + 1;
    if(!(copy = malloc(size))) err(1, 0);
    memcpy(copy, str, size);
    return(copy);
}

static int indirectstrcmp(void const *a, void const *b) {
    return(strcmp(*(char const **)(a), *(char const **)(b)));
}

enum { pathgrow = 64 };
static char **pathv;
static size_t npath;
static char pathbuf[PATH_MAX];

static void add(void) {
    if(!(npath % pathgrow))
        if(!(pathv = realloc(pathv, (npath + pathgrow) * sizeof(char *))))
            err(1, 0);
    pathv[npath++] = xstrdup(pathbuf);
}

static void walk(void) {
    struct stat stbuf;
    struct stat *st = &stbuf;
    struct dirent *d;
    DIR *dirp;
    char *sep;
    sep = strchr(pathbuf, 0);
    if(!(dirp = opendir(*pathbuf ? pathbuf : "."))) {
        warn("cannot open directory %s", pathbuf);
        return;
    }
    while((d = readdir(dirp))) {
        if(!strcmp(d->d_name, ".") || !strcmp(d->d_name, "..")) continue;
        if(strchr(d->d_name, '\n')) continue;
        if((strlcat(pathbuf, *pathbuf ? "/" : "", sizeof(pathbuf)) >= sizeof(pathbuf)) ||
           (strlcat(pathbuf, d->d_name, sizeof(pathbuf)) >= sizeof(pathbuf))) {
            warnx("path too long: %s", d->d_name);
            continue;
        }
        if(stat(pathbuf, st) == -1) {
            warn("cannot stat %s", pathbuf);
            continue;
        }
        if(S_ISDIR(st->st_mode)) {
            walk();
        } else {
            add();
        }
        *sep = 0;
    }
    closedir(dirp);
}

static void emit(void) {
    char **pathp;
    FILE *tempio;
    int tempfd;
    strlcpy(pathbuf, "/tmp/mvedit.XXXXXXXXXX", sizeof(pathbuf));
    if((tempfd = mkstemp(pathbuf)) == -1) err(1, "cannot create temporary file");
    if(!(tempio = fdopen(tempfd, "w"))) err(1, "fdopen");
    qsort(pathv, npath, sizeof(char *), indirectstrcmp);
    for(pathp = pathv; pathp < pathv + npath; ++pathp)
        if(fprintf(tempio, "%s\n", *pathp) == -1)
            err(1, "fprintf");
    fclose(tempio);
}

static void edit(void) {
    char *argv[3];
    pid_t pid;
    int status;
    argv[0] = getenv("EDITOR");
    if(!argv[0]) argv[0] = "vi";
    argv[1] = pathbuf;
    argv[2] = 0;
    if((pid = fork()) == -1) err(1, "fork");
    if(!pid) {
        execvp(argv[0], argv);
        warn("cannot run %s", argv[0]);
        _exit(126);
    }
    if(waitpid(pid, &status, 0) == -1) err(1, "waitpid");
    if(!WIFEXITED(status) || WEXITSTATUS(status)) errx(1, "editor failed");
}

static int readpath(FILE *io) {
    size_t n;
    int c;
    n = 0;
    for(;;) {
        c = fgetc(io);
        if(ferror(io)) err(1, "read error");
        if(!c) errx(1, "null byte in input");
        if(c == EOF) break;
        if(c == '\n') break;
        if(n >= sizeof(pathbuf) - 1) errx(1, "input path too long");
        pathbuf[n++] = c;
    }
    pathbuf[n] = 0;
    return(n > 0);
}

static int yes(void) {
    int ans;
    for(;;) {
        ans = fgetc(stdin);
        if(strchr("yn", ans) && (fgetc(stdin) == '\n')) return(ans == 'y');
        warnx("please answer y for yes or n for no");
    }
}

static void move(void) {
    FILE *tempio;
    char **oldpathp;
    char *oldpath;
    if(!(tempio = fopen(pathbuf, "r"))) err(1, "fopen");
    oldpathp = pathv;
    while(readpath(tempio)) {
        if(oldpathp >= pathv + npath) err(1, "too many paths in input");
        oldpath = *oldpathp++;
        if(!strcmp(oldpath, pathbuf)) continue;
        printf("Old: %s\n", oldpath);
        printf("New: %s\n", pathbuf);
        printf("Rename? ");
        if(yes())
            if(rename(oldpath, pathbuf) == -1)
                warn("cannot rename");
        printf("\n");
    }
    if(oldpathp < pathv + npath) err(1, "too few paths in input");
    fclose(tempio);
}

int main(void) {
    walk();
    emit();
    edit();
    move();
    return(0);
}
