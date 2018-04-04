#! /usr/bin/env python3

import os
import re
import subprocess

os.chdir(os.path.dirname(__file__))
for name in os.listdir('.'):
    if re.match(r'^[a-z]\.png$', name):
        letter = name[0]

        def sh(x): subprocess.check_call(
            x.format(letter=letter), shell=True)
        sh('convert {letter}.png {letter}.tga')
        sh('gzip -1 <{letter}.tga >{letter}.tga.gz')
        sh('./snappy-test {letter}.tga >{letter}.tga.snappy')
        tgasize = os.stat(letter+'.tga').st_size
        pngsize = os.stat(letter+'.png').st_size
        snasize = os.stat(letter+'.tga.snappy').st_size
        pngcomp = 100 - int(100.0 * (pngsize / tgasize))
        snacomp = 100 - int(100.0 * (snasize / tgasize))
        print("{} TGA compressed {:-2}% by PNG, {:-2}% by Snappy, difference {}%"
              .format(letter, pngcomp, snacomp, pngcomp - snacomp))
