#! /usr/bin/env python3

import unicodedata


FILENAME = unicodedata.lookup('LATIN SMALL LETTER A WITH DIAERESIS')+'.txt'


def touch(filename):
    with open(filename, "w") as out:
        print("hello", file=out)


for form in ["NFC", "NFD"]:
    print('Trying', form, flush=True)
    try:
        touch(unicodedata.normalize(form, FILENAME))
    except (OSError, UnicodeError) as err:
        print(err)
