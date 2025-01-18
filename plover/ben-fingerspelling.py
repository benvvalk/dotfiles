# Ben's fingerspelling dictionary.
#
# I map one of the "*" keys to "TSDZ" and use it for custom
# fingerspelling chords.  This is similar to the standard
# fingerspelling chords with the following differences:
#
# * All characters can be typed with the left-hand chords, including
# the letters "e", "u", and "i". (New chords are defined for
# "e"/"u"/"i".)
#
# * Spacing control. A space can be added before and/or after the
# output character with the "E" and "U" keys, respectively.
#
# * Capitalization is done with the right-hand "R" key, rather than
# the right-hand "P" key.

import re

LONGEST_KEY = 1

letter = {
    "A": "a",
    "PW": "b",
    "KR": "c",
    "TK": "d",
    "TH": "e",
    "TP": "f",
    "TKPW": "g",
    "H": "h",
    "WR": "i",
    "SKWR": "j",
    "K": "k",
    "HR": "l",
    "PH": "m",
    "TPH": "n",
    "O": "o",
    "P": "p",
    "KW": "q",
    "R": "r",
    "S": "s",
    "T": "t",
    "TKHR": "u",
    "SR": "v",
    "W": "w",
    "KP": "x",
    "KWR": "y",
    "STKPW": "z",
}

def lookup(chord):

    stroke = chord[0]
    # Check for Unique Ender and strip.
    #
    # All strokes handled by this dictionary end with "TSDZ".

    if not stroke.endswith("TSDZ"):
        raise KeyError

    # If "TSDZ" is pressed alone, do the same thing as "*" key
    # (undo the previous stroke)

    if stroke == "-TSDZ":
        return "=undo"

    stroke = stroke[:-4] # strip "TSDZ"

    # Parse capitalization.
    #
    # Right "R" is used to capitalize the output letter.

    is_capitalized = False
    if stroke.endswith("R"):
        is_capitalized = True
    if is_capitalized:
        stroke = stroke[:-1] # stripe right-hand "R"

    # Parse spacing.
    #
    # "E": adds a space before the output letter
    # "U": adds a space after the output letter

    is_space_before = False
    is_space_after = False

    if "E" in stroke:
        is_space_before = True
    if "U" in stroke:
        is_space_after = True

    stroke = stroke.rstrip("EU-")

    # Lookup output character for left-hand fingerspelling chord.

    if not stroke in letter:
        raise KeyError

    output = letter[stroke]

    # Apply capitalization.

    if is_capitalized:
        output = output.upper()

    # Apply spacing/attachment.

    if is_space_before:
        output = "{^ " + output
    else:
        output = "{^" + output

    if is_space_after:
        output = output + " ^}"
    else:
        output = output + "^}"

    return output
