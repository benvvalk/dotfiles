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

    stroke = stroke.replace('-','')

    isReversed = False
    if 'EU' in stroke:
        isReversed = True
        stroke = stroke.replace('EU','')
        stroke = "".join(reversed(stroke))

    if re.fullmatch(r'[0-9]+', stroke) is None:
        raise KeyError

    if isReversed and len(stroke) != 2:
        raise KeyError

    return "{^%s}" % stroke