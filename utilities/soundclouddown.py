#! /usr/bin/env python

"""
Soundcloud downloader
 * http://developers.soundcloud.com/docs/widget
 * https://github.com/soundcloud/Widget-JS-API/wiki

The core is from someone's gist, I just added some convenience.
"""

from __future__ import print_function
from optparse import OptionParser
import codecs
import json
import logging
import re
import sys
import time

try:
    from urllib.request import FancyURLopener
    from urllib.parse import urlencode
except ImportError:  # Python 2
    from urllib import FancyURLopener, urlencode

try:
    unicode
except NameError:
    def unicode(s): return s


class MozillaURLopener(FancyURLopener):
    version = 'Mozilla/5.0 (X11; Linux i686; rv:7.0.1) Gecko/20100101 Firefox/7.0.1'


def reporthook(blocks_read, block_size, total_size):
    percent_done = max(0, min(100, int(float(blocks_read * block_size) / total_size * 100)))
    total_megs = int(round(float(total_size) / (1024 * 1024)))
    sys.stdout.write('\r% 3d%% of %.1fM ' % (percent_done, total_megs))
    sys.stdout.flush()


def basename_from_title(s):
    s = re.sub(r'\s+', ' ', s)
    s = s.strip()
    for junk in '\\/:?*~':
        s = s.replace(junk, '_')
    s = re.sub(r'^\.+', '', s)
    s = re.sub(r'\.+$', '', s)
    return s


def save_json(basename, rootobj):
    filename = basename+'.json'
    logging.info('saving %s (SoundCloud API data)' % (filename,))
    with codecs.open(filename, 'w', 'UTF-8') as output:
        output.write(unicode(json.dumps(rootobj, ensure_ascii=False,
                                        sort_keys=True, indent=4,
                                        separators=(',', ': '))))


def save_desc_maybe(basename, desc):
    filename = basename+'.txt'
    desc = unicode((desc or '').rstrip())
    if not desc:
        logging.info('no clip description found')
    else:
        logging.info('saving %s (clip description)' % (filename,))
        with codecs.open(filename, 'w', 'UTF-8') as output:
            for line in (line.rstrip() for line in desc.splitlines()):
                print(line, file=output)


def save_audio(basename, stream_url):
    filename = basename+'.mp3'
    logging.info('saving %s (audio)' % (filename,))
    logging.debug('stream URL: %s' % (stream_url,))
    filename, headers = \
        MozillaURLopener().retrieve(stream_url, filename, reporthook)
    logging.debug('headers: %s' % (headers,))


def main():

    opparse = OptionParser(
        usage='usage: %prog [options] <url> ...',
        description='Download SoundCloud clips into current directory')
    opparse.add_option('-v', '--verbose',
                       help='emit informational messages (twice for debug messages)',
                       dest='verbose', action='count', default=0)
    opparse.add_option('--with-json', help='save SoundCloud API data into .json file',
                       dest='with_json', action='store_true', default=False)
    opparse.add_option('--no-audio', help='do not save audio into .mp3 file',
                       dest='no_audio', action='store_true', default=False)
    opparse.add_option('--no-description', help='do not save description into .txt file',
                       dest='no_description', action='store_true', default=False)
    opparse.add_option('--no-username', help='do not include SoundCloud username in filenames',
                       dest='no_username', action='store_true', default=False)
    options, urls = opparse.parse_args()
    if not urls:
        opparse.print_help()
        exit(1)

    levels = (logging.WARNING, logging.INFO, logging.DEBUG)
    logging.basicConfig(
        level=levels[min(options.verbose, len(levels)-1)],
        format='%(name)s: %(message)s')

    for url in urls:
        if not any(url.lower().startswith(prefix)
                   for prefix in ('http://soundcloud.com/', 'https://soundcloud.com/')):
            logging.warning('not recognized as a SoundCloud clip URL; skipping: '+url)
            continue
        widget_url = 'http://soundcloud.com/widget.json?'+urlencode({'url': url})
        widget = json.loads(MozillaURLopener().open(widget_url).read().decode("UTF-8"))
        logging.debug(repr(widget))
        if options.no_username:
            title = '%s (SoundCloud)' % (widget['title'],)
        else:
            title = '%s - %s (SoundCloud)' % (widget['user']['username'], widget['title'])
        basename = basename_from_title(title)
        if options.with_json:
            save_json(basename, widget)
        if not options.no_description:
            save_desc_maybe(basename, widget.get('description'))
        if not options.no_audio:
            save_audio(basename, widget['stream_url']) # http://developers.soundcloud.com/docs/widget


if __name__ == '__main__':
    main()
