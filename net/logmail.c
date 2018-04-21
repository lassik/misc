/*
 * This is a crude nevertheless functional replacement for the sendmail binary.
 * It is for those who don't use email otherwise but want programs on their own
 * machine to be able to mail reports to them. This program will simply dump all
 * mail 'sent' through it to a log file of your choosing; you'll be able to
 * inspect it from there when you care.
 */

#define LOGFILENAME "/tmp/sentmail"


#include <sysexits.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/uio.h>
#include <unistd.h>
#include <string.h>
#include <errno.h>
#include <time.h>


int main(int argc, char **argv) {

   char   *fnlog = LOGFILENAME;
   FILE   *flog;
   int     i;
   char    buf[512];
   int     bytes;
   time_t  clock;

   if (!(flog = fopen(fnlog, "a"))) {
      fprintf(stderr, "error: fopen() on log file \"%s\": %s\n",
       fnlog, strerror(errno));
      return EX_SOFTWARE;
   }

   fprintf(flog, "\n\n\nMail sent by uid %d gid %d\n", getuid(), getgid());

   time(&clock);
   strftime(buf, sizeof(buf), "%a %F %T %z", localtime(&clock));
   fprintf(flog, "on %s\n", buf);

   fprintf(flog, "Command line:");
   for (i=0; i<argc; i++) fprintf(flog, " %s", argv[i]);

   fprintf(flog, "\nMessage:\n");
   for (;;) {
      bytes = read(fileno(stdin), &buf, sizeof(buf)-1);
      if (bytes == EINTR) continue;
      else if (bytes < 0) {
         fprintf(stderr, "error: read() on stdin: %s\n", strerror(errno));
         fclose(flog);
         return EX_SOFTWARE;
      }
      else if (bytes > 0) {
         buf[bytes] = '\0';
         if (fputs(buf, flog) != 0) {
            fprintf(stderr, "fputs() on stdout: %s\n", strerror(errno));
            fclose(flog);
            return EX_SOFTWARE;
         }
      }
      else break;
   }

   fprintf(flog, "\n-----\n");

   fclose(flog);

   return EX_OK;

}
