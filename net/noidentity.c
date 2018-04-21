/*
 * noidentity: an absolutely minimal identd.
 * RFC 1413; TCP port 113. Use via ucspi-tcp.
 */


#include <stdio.h>


#define USERNAME   "null"
#define MAXLINELEN 18


int main() {
   char   query_string[MAXLINELEN+1];
   size_t total_read = 0;
   int    last_char;
   do {
      if ((last_char = getchar()) == '\4')
         break;
      else if (last_char == '\r') {
         if (getchar() == '\n'
         &&  total_read >= 3) {
            query_string[total_read] = '\0';
            printf("%s:USERID:UNIX:%s\r\n", query_string, USERNAME);
         }
         break;
      }
      else
         query_string[total_read++] = (char)last_char;
   } while (total_read < sizeof(query_string)-1);
   return 0;
}
