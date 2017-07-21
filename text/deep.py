#! /usr/bin/env python2
from sys import exit
from random import choice, randrange


def err(msg):
   print "Error: "+msg
   exit(1)


cats = {}

cat = None
f = file("deep.rules")
for line in f:
   line = line.strip()
   if line == "": continue
   if line[0] == "#":
      if cat and cats[cat] == []: err("no entries in "+cat)
      if line[1:] == "": err("empty cat name (following "+cat+")")
      cat = line[1:]
      if cats.has_key(cat): err("cat "+cat+" declared twice")
      cats[cat] = []
   elif cat: cats[cat].append(line)
   else: err("entries before category declaration")
f.close()
if not cat: err("no categories.")
if cat == []: err("no entries in "+cat) # check last category
if not cats.has_key("main"): err("no main")


def expand_phrase(cat):
   global cats
   s = ""
   phrase = choice(cats[cat])
   words = phrase.split(" ")
   if len(words) < 1:
      err("while expanding from "+cat+": phrase without words")
   for w in range(len(words)):
      word = words[w]
      if word[0] == "%":
         acat = word[1:]
         if not cats.has_key(acat):
            err("while expanding \""+word+"\" in \""+phrase+"\": no such cat")
         if w > 0: s += " "
         s += expand_phrase(acat)
      else:
         dojoin = ((w == 0) or (".,:;?!".find(word[0]) >= 0))
         if dojoin: s += word
         else: s += " "+ word
   return s


print
for verse in range(randrange(1,5)):
   for line in range(randrange(1,6)):
      p = expand_phrase("main")
      print p[0].upper()+p[1:]
   print
