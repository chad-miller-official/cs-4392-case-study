#!/usr/bin/python3

import sys

from antlr4 import *
from SchemeLexer import SchemeLexer
from SchemeParser import SchemeParser

def main(argv):
    scheme_file = FileStream(argv[0])
    lexer       = SchemeLexer(scheme_file)
    stream      = CommonTokenStream(lexer)
    parser      = SchemeParser(stream)
    tree        = parser.program()

if __name__ == '__main__':
    main(sys.argv[1:])
