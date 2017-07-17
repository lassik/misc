#include <sys/types.h>
#include <sys/stat.h>

#include <dirent.h>
#include <err.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MEBIBYTES(n) ((n) << 20)

#define DISPLAY_MIN_SIZE MEBIBYTES(5)
#define DISPLAY_MAX_NEST 10
#define DISPLAY_NAME_MAX 39

struct dir {
    struct dir *first_child;
    struct dir *next_sibling;
    unsigned long long size;
    char name[DISPLAY_NAME_MAX + 1];
};

static char prog[] = "dirsize";

static void die(char *s) {
    fprintf(stderr, "%s: %s\n", prog, s);
    exit(1);
}

static struct dir *newdir(char *path) {
    struct dir *dir;
    char *name;
    if (!(dir = calloc(1, sizeof(struct dir))))
        die("out of memory");
    name = strrchr(path, '/');
    if(name) ++name; else name = path;
    strncpy(dir->name, name, sizeof(dir->name));  /* may truncate, it's ok */
    return(dir);
}

static int earlier(struct dir *a, struct dir *b) {
    return(a->size > b->size);
}

static void insert(struct dir **where, struct dir *what) {
    for(;; where = &((*where)->next_sibling))
        if(!*where || earlier(what, *where)) {
            what->next_sibling = *where;
            *where = what;
            break;
        }
}

static char path[PATH_MAX];

static struct dir *collect(void) {
    DIR *dirp;
    struct dirent *ent;
    char *ent_name;
    struct stat ent_stat;
    char *sep;
    struct dir *dir;
    struct dir *subdir;
    int r;
    dirp = opendir(path);
    if(!dirp) {
        dir = 0;
        warnx("bad dir");
    } else {
        dir = newdir(path);
        while((ent = readdir(dirp))) {
            ent_name = ent->d_name;
            if(!strcmp(ent_name, ".")) continue;
            if(!strcmp(ent_name, "..")) continue;
            sep = strchr(path, 0);
            if(strlcat(path, "/", sizeof(path)) >= sizeof(path))
                errx(100, "path too long");
            if(strlcat(path, ent_name, sizeof(path)) >= sizeof(path))
                errx(100, "path too long");
            r = lstat(path, &ent_stat);
            if(r == -1) {
                warnx("bad ent: %s", path);
            } else if(S_ISREG(ent_stat.st_mode)) {
                dir->size += ent_stat.st_size;
            } else if(S_ISDIR(ent_stat.st_mode) && (subdir = collect())) {
                dir->size += subdir->size;
                insert(&dir->first_child, subdir);
            }
            *sep = 0;
        }
        closedir(dirp);
    }
    return(dir);
}

static void display(struct dir *dir, unsigned int nest) {
    unsigned int i;
    if(dir) {
        if((dir->size >= DISPLAY_MIN_SIZE) && (nest <= DISPLAY_MAX_NEST)) {
            for(i = 0; i < nest * 2; ++i) putchar(' ');
            printf("%s %lluM\n", dir->name, dir->size / 1024 / 1024);
            display(dir->first_child, nest + 1);
        }
        display(dir->next_sibling, nest);
    }
}

static void usage(void) {
    fprintf(stderr, "usage: %s root\n", prog);
    exit(1);
}

int main(int argc, char **argv) {
    if(argc != 2) usage();
    if(strlcpy(path, argv[1], sizeof(path)) >= sizeof(path)) die("path too long");
    display(collect(), 0);
    return(0);
}
