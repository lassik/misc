#! /usr/bin/env python3

# Read all data from stdin and show it in a GTK TextView.

import os
import sys

import gi

gi.require_version("Gtk", "3.0")
from gi.repository import GLib, Gtk


def read_all_available_bytes(fileno):
    PART = 4096
    whole = b""
    while True:
        part = os.read(fileno, PART)
        whole += part
        if len(part) < PART:
            return whole


class App:
    def __init__(self):
        self.win = Gtk.Window()
        self.win.set_title("Poll stdin")
        self.win.set_default_size(300, 600)
        self.win.connect("delete-event", Gtk.main_quit)
        self.buf = Gtk.TextBuffer()
        self.tv = Gtk.TextView()
        self.tv.set_buffer(self.buf)
        self.scrolled = Gtk.ScrolledWindow()
        self.scrolled.add(self.tv)
        self.win.add(self.scrolled)
        self.win.show_all()
        GLib.io_add_watch(sys.stdin, GLib.IO_IN, self.on_stdin)

    def append(self, text):
        self.buf.insert(self.buf.get_end_iter(), text)

    def on_stdin(self, file, cond):
        bytes = read_all_available_bytes(file.fileno())
        eof = not bytes
        if eof:
            return False
        self.append(bytes.decode("utf-8"))
        return True

    def main(self):
        Gtk.main()


if __name__ == "__main__":
    try:
        App().main()
    except KeyboardInterrupt:
        print()
