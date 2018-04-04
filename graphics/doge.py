#! /usr/bin/env python3

# https://stackoverflow.com/questions/12413645/displaying-an-image-with-pygobject-and-python-3-from-in-memory-data

# ImageMagick: convert doge.jpeg -size 460x460 -depth 8 RGB:doge.raw

import argparse
from array import array

import gi
gi.require_version('Gtk', '3.0')
from gi.repository import Gtk, GdkPixbuf


def get_doge_rgb():
    width, height, pixels = 460, 460, open('doge.raw', 'rb').read()
    return width, height, pixels


def get_xor_rgb():
    width, height, pixels = 200, 200, array('B')
    for y in range(height):
        for x in range(width):
            pixels.append(255 * (y < height//2))
            pixels.append(255 * (x >= width//2))
            pixels.append(255 * ((y < height//2) ^ (x < width//2)))
    return width, height, pixels


def load_rgb(width, height, pixels):
    win = Gtk.Window()
    win.connect('delete-event', Gtk.main_quit)
    win.add(Gtk.Image.new_from_pixbuf(
        GdkPixbuf.Pixbuf.new_from_data(pixels, GdkPixbuf.Colorspace.RGB,
                                       False, 8, width, height, width * 3)))
    win.show_all()
    Gtk.main()


def load_rgb_via_pnm(width, height, pixels):
    pnm = 'P6 {} {} 255 '.format(width, height).encode('ascii') + pixels
    loader = GdkPixbuf.PixbufLoader.new_with_type('pnm')
    loader.write(pnm)
    win = Gtk.Window()
    win.connect('delete-event', Gtk.main_quit)
    win.add(Gtk.Image.new_from_pixbuf(loader.get_pixbuf()))
    loader.close()
    win.show_all()
    Gtk.main()


IMAGE = {
    'doge': get_doge_rgb,
    'xor': get_xor_rgb,
}

FORMAT = {
    'pnm': load_rgb_via_pnm,
    'rgb': load_rgb,
}

if __name__ == '__main__':
    ap = argparse.ArgumentParser()
    ap.add_argument('image', choices=list(sorted(IMAGE.keys())))
    ap.add_argument('format', choices=list(sorted(FORMAT.keys())))
    args = ap.parse_args()
    FORMAT[args.format](*IMAGE[args.image]())
