# Max number of strokes in chords handled by this dictionary.
#
# Note: This is a special global variable that must be set by all
# plover python dictionaries.

LONGEST_KEY = 1

# Left-hand-only fingerspelling chords.

mappings = {
    "KPR": "a",
    "SPW": "b",
    "SKR": "c",
    "STK": "d", # overwrites brief for `and`; use SKPW instead
    "TKP": "e",
    "STP": "f",
    "TKPWH": "g", # use TKPWH so we don't clobber TKPW for `go`
    "SWH": "h", # use SWH so that Alt+S chord (SHO) doesn't clobber `should`
    "SKWH": "i", # mnemonic: chord shape looks like bottom part of "I"
    "SKWR": "j", # changes SKWR* to capital J
    "SKH": "k", # use SKH so we don't clobber SK for `ask`
    "SHR": "l",
    "SPH": "m",
    "STPH": "n",
    "KPH": "o",
    "SP": "p",
    "SKW": "q",
    "SWR": "r", # SR conflicts with `have`; can use SW-R instead of SWR for `somewhere`
    "PWH": "s", # special case: S conflicts with `is`, full-row chord is memorable
    "TPW": "t", # ST conflicts with `is it`; mnemonic: chord shape is a "T"
    "TKH": "u", # mnemonic: chord shape is two vertical lines, like the left/right sides of "U"
    "TWH": "v", # mnemonic: chord has "V" shape
    "SW": "w",
    "SKP": "x", # overwrites brief for `and`; use built-in STKP instead
    "KWHR": "y", # SKWR conflicts with `j`
    "STKPW": "z" # changes STKPW* to capital Z
    }

def lookup(strokes):

    assert len(strokes) <= LONGEST_KEY

    prefix = ""
    suffix = ""

    shift = False
    control = False
    alt = False
    in_prefix = True

    for c in strokes[0]:
        if c == "*":
            shift = True
            in_prefix = False
        elif c == "-":
            in_prefix = False
        elif c == "A":
            control = True
            in_prefix = False
        elif c == "O":
            alt = True
            in_prefix = False
        elif in_prefix:
            prefix += c
        else:
            suffix += c

    if (not prefix in mappings):
        raise KeyError

    output = mappings[prefix]

    if alt or control:
        # -FP / -PS / -FPS suffices are for adding spaces before/after
        # a literal output character; these suffices are not
        # recognized in combination with control chords (e.g. Ctrl+Z
        # for undo)
        if suffix != '':
            raise KeyError
        if alt:
            output = 'alt(' + output + ')'
        if control:
            output = 'control(' + output + ')'
        if shift:
            output = 'shift(' + output + ')'
        output = '{#' + output + '}'
    else:
        if shift:
            output = output.upper()
        # check for valid suffix (adds leading/trailing space)
        if not suffix in ['', 'FP', 'FPS', 'PS']:
            raise KeyError
        if suffix.startswith('FP'):
            output = ' ' + output
        if suffix.endswith('PS'):
            output = output + ' '
        output = '{^' + output + '^}'

    return output
