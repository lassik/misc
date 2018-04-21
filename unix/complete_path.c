/* tcsh-style pathname completion on unix. */




/* TODO:
 *  -- grok ~username paths
 *  -- give results that fit single- and double-quoted syntax if asked to
 *      (that is, escape spaces and quote chars as needed)
 *  -- fix the spots marked XXX
 *  -- ensure good portability
 */




#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <sys/types.h>
#include <dirent.h>
#include "complete_path.h"




/*============================================================================*
 *                                                                            *
 *   Internal utilities                                                       *
 *                                                                            *
 *============================================================================*/


#define is_abs_path(X) (X[0] == '/')


static char *rel2abspath(const char *base, const char *path) {
   char *abs;
   int baselen;

   /* Keep this check to handle "//foo" paths gracefully. We use
    * strRchr below so it wouldn't catch the first slash and therefore
    * wouldn't realize that this is an absolute path. */
   if (is_abs_path(path))
      return strdup(path);

   baselen = strlen(base);
   abs = malloc(baselen+strlen(path)+2); if (!abs) return NULL;
   strcpy(abs, base);
   if (abs[baselen-1] != '/')
      abs[baselen++] = '/';
   strcpy(&abs[baselen], path);
   return abs;
}


static int common_prefix_len(char **names) {
   int cplen;
   int n, c;

   if (!names || !names[0]) return 0;
   cplen = strlen(names[0]);
   for (n=1; cplen>0 && names[n]!=NULL; n++)
      for (c=0; c<cplen; c++)
         if (names[n][c] != names[n-1][c]) {
            cplen = c;
            break;
         }
   return cplen;
}




/*============================================================================*
 *                                                                            *
 *   Exported routines: complete_path() and friends                           *
 *                                                                            *
 *============================================================================*/


/*
complete_path()

<basepath>
Must be a NULL-terminated string. An absolute base path which will be
used to transform <path> into an absolute path in case it is relative.
That is, <path> is taken to be relative to <basepath>.

<path>
Must be a NULL-terminated string. A relative (or absolute) path that
may include a stub pathname component at the end. The function will
return any possible suffixes for this stub in the <suffixes> array.

<suffixes>
Pointer to a (char **). It will be set to point to a NULL-terminated
array of matching suffix strings if the function succeeds (return
value >= 0). If the function fails, it will be set to NULL. The caller
should free the array when finished with it.

<common_suffix_len>
Pointer to an int-type variable that will receive the length, in
chars, of any common prefix among the suffixes. Eg. if the suffixes
were {"foobar", "foobaz", "fooqux"} then <common_suffix_len> would
become 3, the length of string "foo". If there is no common prefix,
the length will be zero. You can pass a NULL pointer here if you don't
have any use for this information.

Return value:
On success, the number of items in the <suffixes> array,
 excluding the terminating NULL.
On error, one of the error codes, all of which are less than zero.
*/

#define FAIL(ERROR_CODE) {\
   *suffixes = NULL;\
   return ERROR_CODE;\
}

#define ARR_REALLOC\
   if (!(arr = realloc(arr, (++suffix_count) * sizeof(char *)))) {\
      /* XXX clean up here */\
      return COMPLETE_PATH_ENOMEM;\
   }

#define ARR_APPEND\
   foolen = ent->d_namlen-stublen;\
   foo = malloc(foolen+2);\
   strncpy(foo, &ent->d_name[stublen], foolen+2);\
   if (ent->d_type == DT_DIR)\
      foo[foolen] = '/';\
   if (!(arr[suffix_count-1] = foo)) {\
      /* XXX clean up here */\
      return COMPLETE_PATH_ENOMEM;\
   }

int complete_path(char *basepath, char *path,
 char ***suffixes, int *common_suffix_len) {

   char *stub;
   int   stublen;
   char *absprefixdir;
   int   suffix_count;
   DIR  *dir;
   struct dirent *ent;
   char **arr = NULL/*, **arr_tmp to account for realloc() failure XXX */;
   char *foo; int foolen;

   if (!basepath || !path || !suffixes)
      FAIL(COMPLETE_PATH_EARGS)
   if (!is_abs_path(basepath))
      FAIL(COMPLETE_PATH_EBASE)

   /* First, we separate into <stub> the final incomplete portion,
    * if any, of <path>. Then, we store in <absprefixdir> an absolute
    * version of any complete prefix path that precedes <stub>. The
    * prefix needs to be an absolute path so the OS'es file system
    * routines can grok it. */

   stub = strrchr(path, '/');
   if (!stub) { /* only one pathname component; treat the whole path as the stub */
      absprefixdir = basepath;
      stub = path;
   }
   else if (stub == path) { /* an immediate child of the root directory */
      stub++;
      absprefixdir = "/";
   }
   else /* stub > path */ { /* treat the last pathname component as the stub */
      *stub = '\0';
      absprefixdir = rel2abspath(basepath, path);
      *stub = '/'; stub++;
      if (!absprefixdir) FAIL(COMPLETE_PATH_ENOMEM)
   }

   /* We now have an absolute, complete prefix directory path in
    * <absprefix>. Now see whether it actually exists, and if so,
    * read its list of file names and put into <suffixes> the tails
    * of those that have <stub> as a prefix. We use readdir() because
    * of some brain-damaged inflexibility in the design of scandir(). */

   dir = opendir(absprefixdir);
   if (!dir) FAIL(COMPLETE_PATH_ODERRNO)
   suffix_count = 0;
   stublen = strlen(stub);
   if (stublen > 0) {
      /* we have a stub; got to compare each dirent to it */
      while ((ent = readdir(dir))) {
         if (ent->d_namlen < stublen) continue; /* too short to be a match */
         if (!strncmp(ent->d_name, stub, stublen)) {  /* we have a match */
            ARR_REALLOC
            ARR_APPEND
         }
      }
   }
   else {
      /* no stub; just pour all ents into the array without comparison */
      while ((ent = readdir(dir))) {
         ARR_REALLOC
         ARR_APPEND
      }
   }
   closedir(dir);

   /* append a NULL pointer to terminate the array */
   ARR_REALLOC
   arr[--suffix_count /* sorry. */] = NULL;

   if (common_suffix_len)
      *common_suffix_len = common_prefix_len(arr);

   *suffixes = arr;

   return suffix_count;

}


/* complete_path_free()
 * An easy way to free the <suffixes> array returned by complete_path().
 * Note that the arg is type (char **), not (char ***) as in the former. */

void complete_path_free(char **suffixes) {
   int i;
   if (!suffixes) return;
   for (i=0; suffixes[i] != NULL; i++)
      free(suffixes[i]);
   /* free(suffixes); XXX */
}


/* complete_path_strerror()
 * When complete_path() returns <0, throw the return value at
 * this function to get a pointer to a "nice" explanatory
 * error message string. */

char *complete_path_strerror(const int error_code) {
   char *s;
   switch (error_code) {
      case COMPLETE_PATH_EARGS   : return strdup("NULL required argument(s) passed");
      case COMPLETE_PATH_EBASE   : return strdup("Relative base path");
      case COMPLETE_PATH_ENOMEM  : return strdup("Out of memory");
      case COMPLETE_PATH_ODERRNO :
         asprintf(&s, "Cannot open prefix directory:\n%s", strerror(errno));
         return s;
      default: return strdup("Unknown error (this shouldn't happen...)");
   }
}
