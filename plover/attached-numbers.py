# This is a Plover Python dictionary that changes Plover's default
# behaviour when typing numbers, so that numbers are "attached" by
# default. In other words, a space is not automatically inserted
# before a number.
#
# My main motivation for doing is to prevent Plover's automatic
# insertion of spaces from interfering with the use of number
# arguments in vim and emacs.

import re

LONGEST_KEY = 1

def lookup(key):

    stroke = key[0]

    space_before = False
    space_after = False

    if 'K' in stroke:
        stroke = stroke.replace('K','')
        space_before = True

    if 'R' in stroke:
        # We only want to match the left 'R' key here.  (I map the
        # combination of the number key and the right 'R' key
        # to the Alt key in `emily-modifiers.py`.)
        if not '-' in stroke or stroke.index('-') > stroke.index('R'):
            stroke = stroke.replace('R', '', 1)
            space_after = True

    if 'D' in stroke:
        stroke = stroke.replace('D','')
        stroke = stroke + stroke

    if 'Z' in stroke:
        stroke = stroke.replace('Z','')
        stroke = "".join(reversed(stroke))

    stroke = stroke.replace('-','')

    if re.fullmatch(r'[0-9]+', stroke) is None:
        raise KeyError

    output = "{^"
    if space_before:
        output += " "
    output += "%s" % stroke
    if space_after:
        output += " "
    output += "^}"

    return output
