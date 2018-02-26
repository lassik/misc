#! /usr/bin/env python3

# Find out about updates to your favorite games in MAME.
#
# Parses mame's XML output to find the sourcefiles having to do with
# you favorite games in the MAME source code. Then looks in the MAME
# Git log for updates to those sourcefiles. Unfortunately the update
# log is very detailed so I don't know if this is the best approach.

import os
import subprocess
import xml.etree.ElementTree as ET

XML = os.path.expanduser('~/mame.xml')  # `mame -listxml` output
GIT = os.path.expanduser('~/persist/public/src/mame')
GAMES = os.path.splitext(__file__)[0] + '.txt'
DEFAULTDIR = 'src/mame/drivers'

games = set(filter(None, open(GAMES).read().split()))

sourcefiles = set()
machines_done = set()
machine_queue = games

mame = ET.fromstring(open(XML, encoding='UTF-8').read())
while machine_queue:
    for machine in mame.findall('./machine'):
        sourcefile = machine.get('sourcefile')
        if '/' not in sourcefile:
            sourcefile = '/'.join([DEFAULTDIR, sourcefile])
        name = machine.get('name')
        if name in machine_queue:
            for device_ref in machine.findall('./device_ref'):
                device_name = device_ref.get('name')
                if device_name not in machines_done:
                    machine_queue.add(device_name)
            sourcefiles.add(sourcefile)
            machines_done.add(name)
            machine_queue.remove(name)
for sourcefile in sorted(sourcefiles):
    print(sourcefile)
os.chdir(GIT)
subprocess.check_call(
    ['git', 'log', '--date=short', '--pretty=format:%cd %s', '--'] +
    list(sorted(sourcefiles)))
