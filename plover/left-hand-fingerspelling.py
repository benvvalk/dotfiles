import re

# Max number of strokes in chords handled by this dictionary.
#
# Note: This is a special global variable that must be set by all
# plover python dictionaries.

LONGEST_KEY = 1

# Regex for recognizing when one or more right-hand steno keys are
# pressed.

right_hand_keys = re.compile(r'([EUFBLGDZ6789])|([-*AEOU].*[RPTS])')

# Left-hand-only fingerspelling chords.

chars = {
    "SP": " ",
    "SKH": "/",
    "TPW": "\t",
    "KHR": ":",
    "STR": "a",
    "PWR": "b",
    "KPR": "c",
    "STK": "d",
    "SKR": "e",
    "STP": "f",
    "TKPWR": "g",
    "SH": "h",
    "STKPWR": "i",
    "SKWR": "j",
    "PWH": "k",
    "SHR": "l",
    "SPH": "m",
    "STPH": "n",
    "SPR": "o",
    "P": "p",
    "SKW": "q",
    "PR": "r",
    "SWH": "s",
    "TW": "t",
    "SWR": "u",
    "KPH": "v",
    "SW": "w",
    "KPW": "x",
    "TKWR": "y",
    "STKPW": "z"
    }

# Plover requires using special names for non-alphanumeric characters
# in keyboard shortcuts, e.g."{#alt(slash)}" for Alt+/.

shortcut_chars = {
    "SP": "space",
    "SKH": "slash",
    "TPW": "tab",
    "KHR": "colon",
    "SKWH": "escape"
    }

def lookup(key):

    assert len(key) <= LONGEST_KEY

    if right_hand_keys.findall(key[0]):
        raise KeyError

    char = ""

    for k in key[0]:
        if k in "AO-*":
            break
        char += k

    if (not char in chars) and (not char in shortcut_chars):
        raise KeyError

    shift = "*" in key[0]

    mods = []

    if "A" in key[0]:
        mods.append("control")
    if "O" in key[0]:
        mods.append("alt")

    # Control characters (e.g. Escape) are characters that have no
    # printable representation. Such characters have an entry
    # in `shortcut_chars`, but no entry in `chars`.

    control_char = (char in shortcut_chars) and (not char in chars)

    shortcut = mods or control_char

    if shortcut:
        output = shortcut_chars[char]
    else:
        output = chars[char]

    for mod in mods:
        output = mod + "(" + output + ")"

    if shift:
        if shortcut:
            output = mod + 'shift(' + output + ')'
        else:
            output = output.upper()

    if shortcut:
        output = '{^}{#' + output + '}{^}'
    else:
        output = '{^' + output + '^}'

    return output
