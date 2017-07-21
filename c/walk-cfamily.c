#include <ctype.h>
#include <err.h>
#include <stdio.h>
#include <string.h>

static void failure_eof(void)
{
  errx(111,"eof");
}

/*
 * excluded identifiers are those that appear so frequently that
 * indexing them would not convey any useful information.  currently
 * this is just the list of reserved words, but other identifiers
 * might be added on an empirical basis.
 */
static char *exclude[] = {
  /* c pre-processor directives */
  "define",
  "defined",
  "endif",
  "ifdef",
  "ifndef",
  "include",
  /* c reserved words */
  "break",
  "case",
  "char",
  "const",
  "continue",
  "do",
  "double",
  "else",
  "enum",
  "extern",
  "float",
  "for",
  "goto",
  "if",
  "int",
  "long",
  "register",
  "return",
  "short",
  "signed",
  "static",
  "struct",
  "switch",
  "typedef",
  "union",
  "unsigned",
  "void",
  "volatile",
  "while",
  /* c++ reserved words */
  "class",
  "delete",
  "new",
  "public",
  /* 0 terminates the list */
  0
};

static unsigned int exclude_p(char *s)
{
  char **scan;

  for(scan = exclude; *scan; ++scan) if(!strcmp(*scan,s)) return(1);
  return(0);
}

static char buf[120];
static size_t buf_skip = 0;
static size_t buf_fill = 0;

static void clear(void)
{
  buf_skip = 0;
  buf_fill = 0;
}

static void acc(char ch)
{
  if(!buf_skip) {
    if(buf_fill < sizeof(buf) - 1) {
      buf[buf_fill++] = ch;
    } else {
      buf[buf_fill] = 0;
      warnx("token too long, skipping.  token starts: %s",buf);
      buf_skip = 1;
    }
  }
}

static void emit(char *s)
{
  buf[buf_fill] = 0;
  if((buf_fill > 3) && !buf_skip && !exclude_p(buf)) {
    printf("%s %s\n",s,buf);
  }
}

static char c0 = 0;  /* last char */
static char c1 = 0;  /* this char */

static char c(void)
{
  signed int i;

  c0 = c1;
  i = fgetc(stdin);
  if(i == EOF) failure_eof();
  c1 = (char)(i & 0xff);
  return(c1);
}

static void parse_comment_slash_star()
{
  clear();
  for(;;) {
    c();
    if(c0 == '*') if(c1 == '/') break;
    if(c0) acc(c0);
  }
  emit("comment");
}

static void parse_comment_slash_slash()
{
  clear();
  for(;;) {
    if(c() == '\n') break;
    acc(c1);
  }
  emit("comment");
}

static void parse_literal_string_like(char term)
{
  clear();
  for(;;) {
    c();
    if(c1 == term) break;
    if(c1 == '\\') {
      acc(c1);
      c();
    }
    acc(c1);
  }
  emit("string");
}

static void parse_identifier(void)
{
  clear();
  acc(c1);
  for(;;) {
    c();
    if((c1 == '_') || isalnum(c1)) acc(c1); else break;
  }
  emit("identifier");
}

static void parse(void)
{
  for(;;) {
    c();
    if(c1 == '\'') {
      parse_literal_string_like('\'');
    } else if(c1 == '"') {
      parse_literal_string_like('"');
    } else if((c0 == '/') && (c1 == '*')) {
      parse_comment_slash_star();
    } else if((c0 == '/') && (c1 == '/')) {
      parse_comment_slash_slash();
    } else if((c1 == '_') || isalpha(c1)) {
      parse_identifier();
    }
  }
}

signed int main(void)
{
  parse();
  return(0);
}
