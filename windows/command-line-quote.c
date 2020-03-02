// C conversion of the C++ code in:
//
// https://blogs.msdn.microsoft.com/twistylittlepassagesallalike/2011/04/23/everyone-quotes-command-line-arguments-the-wrong-way/

#include <stdio.h>
#include <string.h>

static int should_quote_arg(const char *arg) {
  if (!*arg) {
    return 1;
  }
  for (; *arg; arg++) {
    if (strchr(" \t\n\v\"", *arg)) {
      return 1;
    }
  }
  return 0;
}

static void print_backslashes(int n) {
  for (; n > 0; n--) {
    printf("\\");
  }
}

static void print_quoted_arg(const char *arg) {
  int ch, backslashes;
  printf("\"");
  do {
    ch = *arg++;
    backslashes = 0;
    while (ch == '\\') {
      ch = *arg++;
      backslashes++;
    }
    if (!ch) {
      // Escape all backslashes, but let the terminating double
      // quotation mark we add below be interpreted as a
      // metacharacter.
      print_backslashes(backslashes * 2);
      break;
    } else if (ch == '"') {
      // Escape all backslashes and the following double quotation
      // mark.
      print_backslashes(backslashes * 2 + 1);
      printf("%c", ch);
    } else {
      // Backslashes aren't special here.
      print_backslashes(backslashes);
      printf("%c", ch);
    }
  } while (ch);
  printf("\"");
}

static void print_arg(const char *arg) {
  if (should_quote_arg(arg)) {
    print_quoted_arg(arg);
  } else {
    printf("%s", arg);
  }
}

static void print_args(char **argv) {
  if (*argv) {
    print_arg(*argv++);
  }
  while (*argv) {
    printf(" ");
    print_arg(*argv++);
  }
}

int main(int argc, char **argv) {
  (void)argc;
  print_args(&argv[1]);
  printf("\n");
  return 0;
}
