# Emily's Modifier Dictionary
import re

LONGEST_KEY = 1

# fingerspelling dictionary entries for relevant theories 
spelling = {
        "A"     : "a",
        "PW"    : "b",
        "KR"    : "c",
        "TK"    : "d",
        "TH"    : "e",
        "TP"    : "f",
        "TKPW"  : "g",
        "H"     : "h",
        "EU"    : "i",
        "AOEU"    : "i", # magnum
        "SKWR"  : "j",
        "SKWRAEU" : "j", # magnum
        "K"     : "k",
        "HR"    : "l",
        "PH"    : "m",
        "TPH"   : "n",
        "O"     : "o",
        "P"     : "p",
        "KW"    : "q",
        "R"     : "r",
        "S"     : "s",
        "T"     : "t",
        "TKHR"  : "u",
        "SR"    : "v",
        "W"     : "w",
        "KP"    : "x",
        "KWR"   : "y",
        "STKPW" : "z",
        "STKPWHR" : "z", # magnum 
        }

# same as emily-symbols format, but modified for use on the left hand
symbols = {
        "TR"    : ["tab", "delete", "backspace", "escape"],
        "KPWR"  : ["up", "left", "right", "down"],
        "KPWHR" : ["page_up", "home", "end", "page_down"],
        ""      : ["", "tab", "return", "space"],

        # typable symbols
        "HR"     : ["exclam", "", "notsign", "exclamdown"],
        "PH"     : ["quotedbl", "", "", ""],
        "TKHR"   : ["numbersign", "registered", "copyright", ""],
        "KPWH"   : ["dollar", "euro", "yen", "sterling"],
        "PWHR"   : ["percent", "", "", ""],
        "SKP"    : ["ampersand", "", "", ""],
        "H"      : ["apostrophe", "", "", ""],
        "TPH"    : ["parenleft", "less", "bracketleft", "braceleft"],
        "KWR"    : ["parenright", "greater", "bracketright", "braceright"],
        "T"      : ["asterisk", "section", "", "multiply"],
        "K"      : ["plus", "paragraph", "", "plusminus"],
        "W"      : ["comma", "", "", ""],
        "TP"     : ["minus", "", "", ""],
        "R"      : ["period", "periodcentered", "", ""],
        "WH"     : ["slash", "", "", "division"],
        "TK"     : ["colon", "", "", ""],
        "WR"     : ["semicolon", "", "", ""],
        "TKPW"   : ["equal", "", "", ""],
        "TPW"    : ["question", "", "questiondown", ""],
        "TKPWHR" : ["at", "", "", ""],
        "PR"     : ["backslash", "", "", ""],
        "KPR"    : ["asciicircum", "guillemotleft", "guillemotright", "degree"],
        "KW"     : ["underscore", "", "", "mu"],
        "P"      : ["grave", "", "", ""],
        "PW"     : ["bar", "", "", "brokenbar"],
        "TPWR"   : ["asciitilde", "", "", ""]
        }

def lookup(chord):

    # extract the chord for easy use
    stroke = chord[0]

    # quick tests to avoid regex if non-relevant stroke is sent
    if len(chord) != 1:
        raise KeyError
    assert len(chord) <= LONGEST_KEY

    # Normalize chords that use the number key. In Plover's dictionary
    # format, whenever a chord contains the '#' key, the other keys
    # in the chord must written as numbers rather than letters,
    # when possible. For example, the chord "#STW-F" would
    # be represented as "12W-6". (The "W" key has no corresponding
    # number so it is left unchanged.)
    #
    # However, I want the opposite here -- I wants the keys in the
    # chord to be written as letters. Otherwise, the chord lookup
    # tables at the top of this file will not work.
    #
    # Based on a code block from:
    # https://github.com/EPLHREU/emily-symbols/blob/main/emily-symbols.py

    if any(k in stroke for k in "1234506789"):  # if chord contains a number

        # replace numbers with letters
        stroke = list(stroke)
        numbers = ["O", "S", "T", "P", "H", "A", "F", "P", "L", "T"]
        for key in range(len(stroke)):
            if stroke[key].isnumeric():
                stroke[key] = numbers[int(stroke[key])]  # set number key to letter
        stroke = ["#"] + stroke
        stroke = "".join(stroke)

    # require the number key to be pressed
    if "#" not in stroke:
        raise KeyError

    # extract relevant parts of the stroke
    firstMatch = re.fullmatch(r'#([STKPWHR]*)([AO]*)([*-]?)([EU]*)([RBGS]+)', stroke)

    # error out if there are no matches found
    if firstMatch is None:
        raise KeyError
    # name the relevant extracted parts of the regex
    (key, vowel1, seperator, vowel2, modifiers) = firstMatch.groups()

    # if the user doesn't specify a modifier, then error out as the dictionary has no use otherwise
    if modifiers is None:
        raise KeyError

    # combine the relevant parts of the stroke into a nice name
    pattern = key + vowel1 + vowel2

    # use * to distinguish symbol input from numerical or character input
    if "*" in seperator:
        # symbol input
        # extract the part of the symbol input
        secondMatch = re.fullmatch(r'([STKPWHR]*)([AO]*)([EU]*)', pattern)
        # into variables
        (pattern, variants, vowel2) = secondMatch.groups()
        # if the pattern is not recognised, error out
        if pattern not in symbols:
            raise KeyError

        # calculate the variant count
        variant = 0
        if 'A' in variants:
            variant = variant + 1
        if 'O' in variants:
            variant = variant + 2

        # get the entry 
        entry = symbols[pattern]
        if type(entry) == list:
            extract = entry[variant]
            # error out if the entry isn't applicable
            if extract == "":
                raise KeyError

            character = extract
        else:
            character = entry
    else:
        # numbers or letters
        # extract relevant parts of the stroke
        secondMatch = re.fullmatch(r'([STKPWHR]*)([AO]*)([-EU]*)', pattern)
        (shape, number, vowel2) = secondMatch.groups()

        # AO is unused in finger spelling, thus used to disginguish numerical input
        if number == "AO" and vowel2 == "":

            # left-hand bottom row counts in binary for numbers 0-9
            count = 0
            if "R" in shape:
                count = count + 1
            if "W" in shape:
                count = count + 2
            if "K" in shape:
                count = count + 4
            if "S" in shape:
                count = count + 8

            # if TP is being held as well, then user is inputting a Fx key - like alt+F4
            function = False
            if "T" in shape and "P" in shape:
                function = True

            # add the 'F' if F number
            if function:
                character = "F" + str(count)
                if count > 12:
                    raise KeyError
            else:
                if count > 9:
                    raise KeyError
                character = str(count)
        else:
            # finger spelling input
            entry = shape + number + vowel2

            # check for entry in dictionary 
            if entry not in spelling:
                raise KeyError
            character = spelling[entry]

            # if there is no entry, pass the error
            if character == "":
                raise KeyError

    # accumulate list of modifiers to be added to the character
    # may need to reorder?
    modKeys = modifiers
    mods = []
    if "R" in modKeys:
        mods.append("alt")
    if "B" in modKeys:
        mods.append("super")
    if "G" in modKeys:
        mods.append("control")
    if "S" in modKeys:
        mods.append("shift")

    # apply those modifiers
    combo = character
    for mod in mods:
        combo = mod + "(" + combo + ")"

    # package it up with the syntax
    ret = "{#" + combo + "}{^}"

    # all done! :D
    return ret
