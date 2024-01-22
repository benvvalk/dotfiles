# This script is copied from the example at
# https://pypi.org/project/plover-python-dictionary/
#
# When the user types the stroke "STR*", the next stroke will be
# printed verbatim instead of being translated. This is a handy tool
# debugging, particularly when developing new Plover Python
# dictionaries.

LONGEST_KEY = 2

SHOW_STROKE_STENO = 'STR*'

def lookup(key):
    assert len(key) <= LONGEST_KEY, '%d/%d' % (len(key), LONGEST_KEY)
    if SHOW_STROKE_STENO != key[0]:
        raise KeyError
    if len(key) == 1:
        return ' '
    return key[1]
