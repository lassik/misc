#! /usr/bin/env python3

import colorsys
import os
import xml.etree.ElementTree as ET


def hexcolor(r_g_b):
    return "#{:02x}{:02x}{:02x}".format(*(int(round(255 * x)) for x in r_g_b))


def notecolor(byte, octave):
    h = float(byte) / 127
    s = 0.3
    v = 0.8 if octave % 2 == 0 else 0.9
    return colorsys.hsv_to_rgb(h, s, v)


outfile = os.path.splitext(__file__)[0] + ".html"
html = ET.Element("html")
head = ET.SubElement(html, "head")
style = ET.SubElement(head, "style").text = (
    "html { font-family: sans-serif }"
    "td, th { border: 1px solid black; text-align: left; width: 30px; padding-left: 10px; padding-right: 10px; }"
    "tr.even td, tr.even th { background-color: beige; }"
)
body = ET.SubElement(html, "body")
h1 = ET.SubElement(body, "h1").text = "MIDI Notes"
table = ET.SubElement(body, "table")
tr = ET.SubElement(table, "tr")
ET.SubElement(tr, "th").text = "Dec"
ET.SubElement(tr, "th").text = "Hex"
ET.SubElement(tr, "th").text = "Note"
ET.SubElement(tr, "th").text = "Note"
tr = ET.SubElement(table, "tr")
sharps = ["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"]
flats = ["C", "Db", "D", "Eb", "E", "F", "Gb", "G", "Ab", "A", "Bb", "B"]

for byte in range(0, 127 + 1):
    octave, note = divmod(byte, 12)
    octave -= 1
    sharp = sharps[note] + str(octave)
    flat = flats[note] + str(octave)
    tr = ET.SubElement(table, "tr")
    tr.attrib["style"] = "background-color: " + hexcolor(notecolor(byte, octave))
    ET.SubElement(tr, "td").text = "{:03d}".format(byte)
    ET.SubElement(tr, "td").text = "0x{:02x}".format(byte)
    ET.SubElement(tr, "td").text = sharp
    ET.SubElement(tr, "td").text = flat

ET.ElementTree(html).write(open(outfile, "w"), encoding="unicode")
