// Write all the error messages in the given locale.

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <locale.h>

int
main(int argc, char **argv)
{
    const char *locale_name;
    locale_t locale;
    int i;

    if (argc != 2) {
        fprintf(stderr, "usage: strerror-locale locale\n");
        exit(EXIT_FAILURE);
    }
    locale_name = argv[1];
    if (!(locale = newlocale(LC_MESSAGES_MASK, locale_name, 0))) {
        fprintf(stderr, "newlocale(): %s\n", strerror(errno));
        exit(EXIT_FAILURE);
    }
    for (i = 1; i < 300; i++) {
        printf("%s\n", strerror_l(i, locale));
    }
    freelocale(locale);
    return EXIT_SUCCESS;
}
