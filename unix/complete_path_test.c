#include <sys/types.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <readline/readline.h>

#include "complete_path.h"

int main() {
   char wd[1024];
   char *input;
   char **suffixes;
   int comsuflen;
   int ret, i;
   getcwd(wd, sizeof(wd)-1);
   for (;;) {
      input = readline("Test: ");
      if (!strcmp(input, "q")) {
         free(input);
         break;
      }
      else {
         ret = complete_path(wd, input, &suffixes, &comsuflen);
         if (ret > 0) {
            printf("Match count: %d\n", ret);
            for (i=0; i<ret; i++)
               printf("%03d: %s\n", i, suffixes[i]);
            printf("Common suffix len: %d\n", comsuflen);
         }
         else if (ret == 0)
            printf("No matches.\n");
         else
            printf("Error: %s\n", complete_path_strerror(ret));
         complete_path_free(suffixes);
         free(input);
      }
   }
   return 0;
}
