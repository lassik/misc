#! /usr/bin/env python3

# Extract information about IANA character sets (charset) from IANA's
# own .csv file.
#
# https://www.iana.org/assignments/character-sets/
# https://www.iana.org/assignments/character-sets/character-sets-1.csv

import os
import csv
from collections import namedtuple


CSV = os.path.join(os.path.splitext(__file__)[0] + ".csv")
Row = namedtuple("Row", "preferred_mime_name name mib source reference aliases note")


def group_ints(maxgroupsize, ints):
    groups = []
    group = []
    for int_ in sorted(set(ints)):
        if group and int_ - group[0] >= maxgroupsize:
            groups.append(group)
            group = []
        group.append(int_)
    if group:
        groups.append(group)
    return groups


def csv_rows():
    with open(CSV, newline="") as csvfile:
        sample = csvfile.read(1024)
        csvfile.seek(0)
        sniffer = csv.Sniffer()
        dialect = sniffer.sniff(sample)
        dialect.doublequote = True
        reader = csv.reader(csvfile, dialect)
        assert sniffer.has_header(sample)
        for row in reader:
            break
        for row in reader:
            row[2] = int(row[2])  # mib
            yield Row(*row)


def print_stats(groups):
    for group in groups:
        print(repr(group))
        print()
    ntotal = 0
    for group in groups:
        n = len(group)
        ntotal += n
        print("n={}".format(n))
    print("ntotal={}".format(ntotal))


if __name__ == "__main__":
    print_stats(group_ints(256, (row.mib for row in csv_rows())))
