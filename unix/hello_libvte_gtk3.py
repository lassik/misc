#! /usr/bin/env python3

# https://bazaar.launchpad.net/~gnome-terminator/terminator/gtk3/view/head:/terminatorlib/terminal.py
# https://earobinson.wordpress.com/2007/09/10/python-vteterminal-example/

import gi

gi.require_version("Gtk", "3.0")
gi.require_version("Vte", "2.91")
from gi.repository import Gtk, GdkPixbuf, Vte


def make_vte():
    win = Gtk.Window()
    win.set_title("VTE Test")
    win.connect("delete-event", Gtk.main_quit)
    win.connect("destroy", Gtk.main_quit)
    vte = Vte.Terminal()
    win.add(vte)
    win.show_all()
    return vte


if __name__ == "__main__":
    vte = make_vte()
    vte.feed(b"\x1b[36mhello \x1b[1mworld :)")
    Gtk.main()
