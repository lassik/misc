#! /usr/bin/env python3


import csv
import glob
import os


def cleanup(s):
    return ' '.join(s.split())


def csv_rows(csvfile):
    with open(csvfile, newline='', encoding='utf-8') as inp:
        reader = csv.reader(inp, delimiter=',', quotechar='"')
        return list(reader)[1:]


def line_from_csv_row(row):
    buyer_lastname, buyer_firstname, guest_fullname = \
        row[5], row[6], cleanup(row[16])
    buyer_fullname = cleanup(
        ' '.join([buyer_firstname, buyer_lastname]))
    assert buyer_fullname
    if guest_fullname and guest_fullname != buyer_fullname:
        return '{} (osti {})'.format(guest_fullname, buyer_fullname)
    else:
        return buyer_fullname


def readlines(csvfiles):
    lines = []
    for csvfile in csvfiles:
        lines.extend(map(line_from_csv_row, csv_rows(csvfile)))
    return list(sorted(lines, key=str.lower))


def writelines(outfile, lines):
    with open(outfile, 'w', encoding='utf-8') as out:
        for line in lines:
            print(line, file=out)


if __name__ == '__main__':
    os.chdir(os.path.dirname(__file__))
    txtfile = os.path.splitext(os.path.basename(__file__))[0] + '.txt'
    writelines(txtfile, readlines(glob.glob('*.csv')))
