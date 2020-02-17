#! /usr/bin/env python

from pygments.lexers import guess_lexer, guess_lexer_for_filename, get_lexer_for_filename
import sys

try:
    filename = "FOO." + str(sys.argv[1])
    print(get_lexer_for_filename(filename).name.replace('EmacsLisp', 'elisp'))
except:
    print("text")
