#include <err.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

struct sb {
    size_t size;
    size_t fill;
    char *data;
};

static struct sb *sb_new()
{
    struct sb *sb;

    sb = malloc(sizeof(struct sb));
    if(!sb) errx(1, "out of memory");
    memset(sb, 0, sizeof(struct sb));  /* important: sb_need() relies on this */
    return(sb);
}

static void sb_need(struct sb *sb, size_t n)
{
    size_t room = sb->size - sb->fill;  /* can't underflow */
    char *new_data;

    if(n > room) {
        if(n > (SIZE_MAX - sb->fill)) {  /* can't underflow */
            errx(1, "out of memory");
        }
        sb->size = sb->fill + n;  /* can't overflow */
        new_data = realloc(sb->data, sb->size);  /* can be realloc() of null pointer */
        if(!new_data) errx(1, "out of memory");
        sb->data = new_data;
    }
}

static void sb_putc(struct sb *sb, int ch)
{
    sb_need(sb, 1);
    sb->data[sb->fill] = ch;
    ++sb->fill;  /* overflow checked in sb_need() */
}

static void sb_putsb(struct sb *sb, struct sb *appendage)
{
    sb_need(sb, appendage->fill);
    memcpy(&sb->data[sb->fill], appendage->data, appendage->fill);
    sb->fill += appendage->fill;  /* overflow checked in sb_need() */
}

static void sb_puts(struct sb *sb, const char *s0)
{
    size_t len = strlen(s0);

    sb_need(sb, len);
    memcpy(&sb->data[sb->fill], s0, len);
    sb->fill += len;  /* overflow checked in sb_need() */
}

static void sb_putd(struct sb *sb, int d)
{
    char x[12];
    char *p;
    int neg;

    if(d < 0) { neg = 1; d = -d; } else neg = 0;
    p = x + sizeof(x);
    *--p = '\0';
    do *--p = '0' + (d % 10); while(d /= 10);
    if(neg) *--p = '-';
    sb_puts(sb, p);
}

static void sb_free(struct sb *sb)
{
    free(sb->data);  /* can be free() of null pointer */
    free(sb);
}

static void html_char(struct sb *sb, int ch)
{
    if((ch != 10) && ((ch < 32) || (ch > 126))) {
        sb_puts(sb, "&#"); sb_putd(sb, ch); sb_putc(sb, ';');
    } else if(ch == '"') {
        sb_puts(sb, "&quot;");
    } else if(ch == '&') {
        sb_puts(sb, "&amp;");
    } else if(ch == '<') {
        sb_puts(sb, "&lt;");
    } else if(ch == '>') {
        sb_puts(sb, "&gt;");
    } else {
        sb_putc(sb, ch);
    }
}

/*
 * note: the code below assumes that EOF is defined as a negative
 * integer that fits in a signed long.  this should be the case on all
 * implementations of stdio.
 */

/*
 * gives an integer value equal to the n low-order bits of the integer
 * value x.  n must be an integer such that 0 <= n < sizeof(x) * 8
 */
#define LOWBITS(x, n) ((x) & ((1 << (n)) - 1))

void getc_utf8_cont(FILE *io, unsigned int nbytes, signed long *pch)
{
    int byte;

    for(; nbytes > 0; --nbytes) {
        byte = getc(io);
        if(byte == EOF) {
            *pch = EOF;
            break;
        } else if((byte >= 0x80) && (byte < 0xC0)) {
            *pch <<= 6;
            *pch |= LOWBITS(byte, 6);
        } else {
            /* xxx: should set ferror here. */
            *pch = '?';
            break;
        }
    }
}

signed long getc_utf8(FILE *io)
{
    signed long ch;
    int byte;

    byte = getc(io);
    if(byte == EOF) {
        /* end-of-file or error. */
        ch = EOF;
    } else if(byte < 0x80) {
        /* first and only byte of a 1-byte utf-8 character. */
        ch = byte;
    } else if(byte < 0xC0) {
        /* continuation byte of a multi-byte utf-8 character. */
        ch = '?';
    } else if(byte < 0xE0) {
        /* first byte of a 2-byte utf-8 character. */
        ch = LOWBITS(byte, 5);
        getc_utf8_cont(io, 1, &ch);
    } else if(byte < 0xF0) {
        /* first byte of a 3-byte utf-8 character. */
        ch = LOWBITS(byte, 4);
        getc_utf8_cont(io, 2, &ch);
    } else if(byte < 0xF8) {
        /* first byte of a 4-byte utf-8 character. */
        ch = LOWBITS(byte, 3);
        getc_utf8_cont(io, 3, &ch);
    } else {
        /* outside the valid utf-8 range. */
        /* xxx: should set ferror here. */
        ch = '?';
    }
    return(ch);
}

static struct sb *rsht_title = NULL;
static unsigned long rsht_last_ln_cols = 0;
static unsigned long rsht_ln = 1;
static unsigned long rsht_col = 1;
static int rsht_lastch = 0;
static int rsht_last_block_chr = 0;

static void rsht_syn_err(const char *format, ...)
{
    va_list ap;

    fprintf(stderr, "On line %ld column %ld:\n", rsht_ln, rsht_col);
    va_start(ap, format);
    vfprintf(stderr, format, ap);
    va_end(ap);
    fprintf(stderr, "\n");
    exit(1);
}

/* xxx: no overflow checks */
static int rsht_getc()
{
    int ch;

    if(rsht_lastch) {
        ch = rsht_lastch;
        rsht_lastch = 0;
    } else {
        ch = getc_utf8(stdin);
    }
    if(ch == '\n') {
        rsht_last_ln_cols = rsht_col - 1;
        ++rsht_ln;
        rsht_col = 1;
    } else {
        ++rsht_col;
    }
    return(ch);
}

/* xxx: no overflow checks */
static void rsht_ungetc(int ch)
{
    rsht_lastch = ch;
    if(ch == '\n') {
        --rsht_ln;
        rsht_col = rsht_last_ln_cols + 1;
    } else if(rsht_col > 0) {  /* Should always be true. */
        --rsht_col;
    }
}

static int rsht_empty_line()
{
    int ch;

    ch = rsht_getc();
    if((ch == '\n') || (ch == EOF)) {
        return(1);
    } else {
        rsht_ungetc(ch);
        return(0);
    }
}

static void rsht_read_inline(struct sb *sb);

static int rsht_read_inline_until(struct sb *sb, int goal_ch_1, int goal_ch_2)
{
    int ch;

    for(;;) {
        ch = rsht_getc();
        if(ch == goal_ch_1) return(goal_ch_1);
        if(ch == goal_ch_2) return(goal_ch_2);
        if(ch == EOF) {
            rsht_syn_err("end of file while expecting %c or %c", goal_ch_1, goal_ch_2);
        }
        rsht_ungetc(ch);
        rsht_read_inline(sb);
    }
}

static void rsht_read_inline_delim(struct sb *sb, int l_ch, int r_ch)
{
    if(rsht_getc() != l_ch) rsht_syn_err("expecting %c", l_ch);
    rsht_read_inline_until(sb, r_ch, EOF);
}

static void rsht_read_inline_bold(struct sb *sb)
{
    sb_puts(sb, "<b>");
    rsht_read_inline_delim(sb, '<', '>');
    sb_puts(sb, "</b>");
}

static void rsht_read_inline_italic(struct sb *sb)
{
    sb_puts(sb, "<i>");
    rsht_read_inline_delim(sb, '<', '>');
    sb_puts(sb, "</i>");
}

static void rsht_read_inline_underline(struct sb *sb)
{
    sb_puts(sb, "<u>");
    rsht_read_inline_delim(sb, '<', '>');
    sb_puts(sb, "</u>");
}

static void rsht_read_inline_link(struct sb *sb)
{
    struct sb *caption = sb_new();
    struct sb *href = NULL;

    if(rsht_read_inline_until(caption, '=', '>') == '=') {
        href = sb_new();
        rsht_read_inline_until(href, '>', EOF);
    }
    sb_puts(sb, "<a href=\"");
    if(href) {
        sb_putsb(sb, href);
    } else {
        sb_putsb(sb, caption);
    }
    sb_puts(sb, "\">");
    sb_putsb(sb, caption);
    sb_puts(sb, "</a>");
    sb_free(caption);
    if(href) sb_free(href);
}

static void rsht_read_inline_image(struct sb *sb)
{
    if(rsht_getc() != '<') rsht_syn_err("< does not follow #g");
    sb_puts(sb, "<img alt=\"");
    rsht_read_inline_until(sb, '=', EOF);
    sb_puts(sb, "\" src=\"");
    rsht_read_inline_until(sb, '>', EOF);
    sb_puts(sb, "\">");
}

static void rsht_read_inline(struct sb *sb)
{
    int ch;

    ch = rsht_getc();
    if(ch == EOF) {
        /* nothing to do */
    } else if(ch == '\\') {
        ch = rsht_getc();
        if(ch == EOF) rsht_syn_err("end of file while expecting escaped char");
        html_char(sb, ch);
    } else if(ch == '#') {
        ch = rsht_getc();
        switch(ch) {
        case 'b': rsht_read_inline_bold(sb); break;
        case 'i': rsht_read_inline_italic(sb); break;
        case 'u': rsht_read_inline_underline(sb); break;
        case '<': rsht_read_inline_link(sb); break;
        case 'g': rsht_read_inline_image(sb); break;
        default: rsht_syn_err("no such inline dispatch macro character: %c", ch);
        }
    } else {
        html_char(sb, ch);
    }
}

static void rsht_read_block_heading(struct sb *sb)
{
    struct sb *text;
    const char *levels = "123456";
    int level;

    rsht_getc();  /* skip the '['. */
    level = rsht_getc();
    if(!strchr(levels, level)) rsht_syn_err("heading block: invalid level: %c", level);
    text = sb_new();
    do {
        if(rsht_getc() != ' ') rsht_syn_err("syntax error: heading block");
        rsht_read_inline_until(text, '\n', EOF);
    } while(!rsht_empty_line());
    sb_puts(sb, "<h");
    sb_putc(sb, level);
    sb_putc(sb, '>');
    sb_putsb(sb, text);
    sb_puts(sb, "</h");
    sb_putc(sb, level);
    sb_puts(sb, ">\n");
    if(level == '1') {
        if(!rsht_title) {
            rsht_title = text;
            return;
        } else {
            warn("document has two level-1 headings");
        }
    }
    sb_free(text);
}

static void rsht_read_block_table(struct sb *sb)
{
    int ch;
    int flag_try;

    sb_puts(sb, "<table border=\"1\">\n");
    do {
        ch = rsht_getc();
        if(ch != '|') rsht_syn_err("line in table block doesn't start with vertical bar");
        sb_puts(sb, "<tr>\n");
        flag_try = 1;
        do {
            sb_puts(sb, "<td>");
            flag_try = (rsht_read_inline_until(sb, '|', '\n') == '|');
            sb_puts(sb, "</td>\n");
        } while(flag_try);
        sb_puts(sb, "</tr>\n");
    } while(!rsht_empty_line());
    sb_puts(sb, "</table>\n");
}

#define LIST_NEST_MAX 20

static void rsht_list_nest(struct sb *sb, int old_nest, int new_nest)
{
    int d;

    d = new_nest - old_nest;
    if(d > 1) {
        rsht_syn_err("list item is more than one nesting level deeper than preceding item");
    } else if(d == 1) {
        sb_puts(sb, "<ul>\n");
    } else {
        for(; d < 0; ++d) sb_puts(sb, "</li>\n</ul>\n");
        if(new_nest > 0) sb_puts(sb, "</li>\n");
    }
}

static void rsht_read_block_list(struct sb *sb)
{
    char styles[LIST_NEST_MAX];
    int old_nest;
    int new_nest;
    int chr;

    old_nest = 0;
    do {
        new_nest = 0;
        for(;;) {
            chr = rsht_getc();
            if(chr != '*') if(chr != '%') break;
            if((new_nest < old_nest) && (styles[new_nest] != chr)) {
                rsht_syn_err("list style mismatch");
            } else if(new_nest == old_nest) {
                styles[new_nest] = chr;
            }
            ++new_nest;
        }
        if(chr != EOF) rsht_ungetc(chr);
        if(!new_nest) rsht_syn_err("non-nested line in list block");
        if(new_nest > LIST_NEST_MAX) rsht_syn_err("list nests deeper than %d levels", LIST_NEST_MAX);
        rsht_list_nest(sb, old_nest, new_nest);
        old_nest = new_nest;
        sb_puts(sb, "<li>\n");
        rsht_read_inline_until(sb, '\n', EOF);
        sb_putc(sb, '\n');
    } while(!rsht_empty_line());
    rsht_list_nest(sb, old_nest, 0);
}

static void handle_pre(struct sb *sb, char chr)
{
    if(rsht_last_block_chr == ' ') {
        if(chr != ' ') {
            sb_puts(sb, "</pre>\n");
        } else {
            sb_putc(sb, '\n');
        }
    } else if(chr == ' ') {
        sb_puts(sb, "<pre>\n");
    }
    rsht_last_block_chr = chr;
}

static void rsht_read_block_preformatted(struct sb *sb)
{
    int ch;

    do {
        ch = rsht_getc();
        if(ch != ' ') rsht_syn_err("line in preformatted block doesn't start with space");
        rsht_read_inline_until(sb, '\n', EOF);
        sb_putc(sb, '\n');
    } while(!rsht_empty_line());
}

static void rsht_read_block_paragraph(struct sb *sb)
{
    sb_puts(sb, "<p>\n");
    do {
        rsht_read_inline_until(sb, '\n', EOF);
        sb_putc(sb, '\n');
    } while(!rsht_empty_line());
    sb_puts(sb, "</p>\n");
}

static void rsht_read_block(struct sb *sb, char chr)
{
    handle_pre(sb, chr);
    switch(chr) {
    case '[': rsht_read_block_heading(sb); break;
    case '|': rsht_read_block_table(sb); break;
    case '*': rsht_read_block_list(sb); break;
    case '%': rsht_read_block_list(sb); break;
    case ' ': rsht_read_block_preformatted(sb); break;
    default : rsht_read_block_paragraph(sb);
    }
}

int main()
{
    struct sb *sb = sb_new();
    int chr;

    for(;;) {
        do chr = rsht_getc(); while(chr == '\n');
        if(chr == EOF) break;
        rsht_ungetc(chr);
        rsht_read_block(sb, chr);
    }
    handle_pre(sb, chr);
    printf("<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\" \"http://www.w3.org/TR/html4/strict.dtd\">\n");
    printf("<html>\n");
    printf("<head>\n");
    printf("<title>");
    if(rsht_title) {
        fwrite(rsht_title->data, 1, rsht_title->fill, stdout);
        sb_free(rsht_title);
    } else {
        warn("document has no level-1 heading");
    }
    printf("</title>\n");
    printf("</head>\n");
    printf("<body>\n");
    fwrite(sb->data, 1, sb->fill, stdout);
    printf("</body>\n");
    printf("</html>\n");
    sb_free(sb);
    return(0);
}
