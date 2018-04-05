#! /usr/bin/env python3

import os
import sys

import gi
gi.require_version('Gtk', '3.0')
from gi.repository import GLib, Gtk


logfile = open(os.path.splitext(__file__)[0]+'.log', 'w')


def on_stdin(fobj, cond):
    line = fobj.readline()
    print(repr(line), file=logfile)
    if line == '':
        sys.exit(0)
    return True


win = Gtk.Window()
win.connect('delete-event', Gtk.main_quit)
win.show_all()
GLib.io_add_watch(sys.stdin, GLib.IO_IN, on_stdin)
Gtk.main()
