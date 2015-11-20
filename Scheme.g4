grammar Scheme;

/* Lexical structure */

token
    : identifier
    | boolean
    | number
    | character
    | string
    | '('
    | ')'
    | '#('
    | '\''
    | '`'
    | ','
    | ',@'
    | '.'
;

delimiter
    : whitespace
    | '('
    | ')'
    | '"'
    | ';'
;

whitespace
    : ' '
    | '\n'
;

comment
    : SEMI .*
;

atmosphere
    : whitespace
    | comment
;

intertokenSpace
    : atmosphere*
;

identifier
    : initial subsequent*
    | peculiarIdentifier
;

initial
    : letter
    | specialInitial
;

letter
    : ('a'..'z')
;

specialInitial
    : '!'
    | '$'
    | '%'
    | '&'
    | '*'
    | '/'
    | ':'
    | '<'
    | '='
    | '>'
    | '?'
    | '^'
    | '_'
    | '~'
;

subsequent
    : initial
    | digit
    | specialSubsequent
;

digit
    : ('0'..'9')
;

specialSubsequent
    : '+'
    | '-'
    | '.'
    | '@'
;

peculiarIdentifier
    : PLUS
    | DASH
    | ELLIPSES
;

syntacticKeyword
    : expressionKeyword
    | 'else'
    | '=>'
    | 'define'
    | 'unquote'
    | 'unquote-splicing'
;

expressionKeyword
    : 'quote'
    | 'lambda'
    | 'if'
    | 'set!'
    | 'begin'
    | 'cond'
    | 'and'
    | 'or'
    | 'case'
    | 'let'
    | 'let*'
    | 'letrec'
    | 'do'
    | 'delay'
    | 'quasiquote'
;

variable
    : identifier
;

boolean
    : '#t'
    | '#f'
;

character
    : '#\\' anyCharacter
    | '#\\' characterName
;

characterName
    : 'space'
    | 'newline'
;

string
    : '"' stringElement* '"'
;

stringElement
    : ~('\\"' | "\\\\" )
    | '\\"'
    | '\\\\'
;

number
    : num2
    | num8
    | num10
    | num16
;

num2
    : prefix2 complex2
;

complex2
    : real2
    | real2 '@' real2
    | real2 '+' ureal2 'i'
    | real2 '-' ureal2 'i'
    | real2 '+ 'i'
    | real2 '-' 'i'
    | '+' ureal2 'i'
    | '-' ureal2 'i'
    | '+' 'i'
    | '-' 'i'
;

real2
    : sign ureal2
;

ureal2
    : uinteger2
    | uinteger2 '/' uinteger2
    | decimal10
;

decimal10
    : uinteger10 suffix
    | '.' digit10+ '#'* suffix
    | digit10+ '.' digit10* '#'* suffix
    | digit10+ '#'+ '.' '#'* suffix
;

uinteger2
    : digit2+ '#'*
;

prefix2
    : radix2 exactness
    | exactness radix2
;

num8
    : prefix8 complex8
;

complex8
    : real8
    | real8 '@' real8
    | real8 '+' ureal8 'i'
    | real8 '-' ureal8 'i'
    | real8 '+ 'i'
    | real8 '-' 'i'
    | '+' ureal8 'i'
    | '-' ureal8 'i'
    | '+' 'i'
    | '-' 'i'
;

real8
    : sign ureal8
;

ureal8
    : uinteger8
    | uinteger8 '/' uinteger8
    | decimal10
;

uinteger8
    : digit8+ '#'*
;

prefix8
    : radix8 exactness
    | exactness radix8
;

num10
    : prefix10 complex10
;

complex10
    : real10
    | real10 '@' real10
    | real10 '+' ureal10 'i'
    | real10 '-' ureal10 'i'
    | real10 '+ 'i'
    | real10 '-' 'i'
    | '+' ureal10 'i'
    | '-' ureal10 'i'
    | '+' 'i'
    | '-' 'i'
;

real10
    : sign ureal10
;

ureal10
    : uinteger10
    | uinteger10 '/' uinteger10
    | decimal10
;

uinteger10
    : digit10+ '#'*
;

prefix10
    : radix10 exactness
    | exactness radix10
;

num16
    : prefix16 complex16
;

complex16
    : real16
    | real16 '@' real16
    | real16 '+' ureal16 'i'
    | real16 '-' ureal16 'i'
    | real16 '+ 'i'
    | real16 '-' 'i'
    | '+' ureal16 'i'
    | '-' ureal16 'i'
    | '+' 'i'
    | '-' 'i'
;

real16
    : sign ureal16
;

ureal16
    : uinteger16
    | uinteger16 '/' uinteger16
    | decimal16
;

uinteger16
    : digit16+ '#'*
;

prefix16
    : radix16 exactness
    | exactness radix16
;

suffix
    :
    | exponentMarker sign digit10+
;

exponentMarker
    : 'e'
    | 's'
    | 'f'
    | 'd'
    | 'l'
;

sign
    :
    | '+'
    | '-'
;

exactness
    :
    | '#i'
    | '#e'
;

radix2
    : '#b'
;

radix8
    : '#o'
;

radix10
    :
    | '#d'
;

radix16
    : '#x'
;

digit2
    : '0'
    | '1'
;

digit8
    : ('0'..'7')
;

digit10
    : digit
;

digit16
    : digit10
    | ('a'..'f')
;

/* External representations */

/* Expressions */

/* Quasiquotations */

/* Transformers */

/* Programs and definitions */

