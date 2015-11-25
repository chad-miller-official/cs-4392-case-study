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
    : ';' (~('\n'))*
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
    : Letter
    | specialInitial
;

Letter
    : [a-z]
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
    | Digit
    | specialSubsequent
;

Digit
    : [0-9]
;

specialSubsequent
    : '+'
    | '-'
    | '.'
    | '@'
;

peculiarIdentifier
    : '+'
    | '-'
    | '...'
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
    : '#\\' .
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
    : ~('"' | '\\')
    | '\\"'
    | '\\\\'
;

number
    : numR[2]
    | numR[8]
    | numR[10]
    | numR[16]
;

numR[int d]
    : prefixR[d] complexR[d]
;

complexR[int d]
    : realR[d]
    | realR[d] '@' realR[d]
    | realR[d] '+' urealR[d] 'i'
    | realR[d] '-' urealR[d] 'i'
    | realR[d] '+' 'i'
    | realR[d] '-' 'i'
    | '+' urealR[d] 'i'
    | '-' urealR[d] 'i'
    | '+' 'i'
    | '-' 'i'
;

realR[int d]
    : sign urealR[d]
;

urealR[int d]
    : uintegerR[d]
    | uintegerR[d] '/' uintegerR[d]
    | decimalR[d]
;

decimalR[int d]
    : {d == 10}? (
          uintegerR[10] suffix
        | '.' digitR[10]+ '#'* suffix
        | digitR[10]+ '.' digitR[10] '#'* suffix
        | digitR[10]+ '#'+ '.' '#'* suffix
      )
;

uintegerR[int d]
    : digitR[d]+ '#'*
;

prefixR[int d]
    : radixR[d] exactness
    | exactness radixR[d]
;

suffix
    :
    | exponentMarker sign digitR[10]+
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

radixR[int d]
    : {d == 2}? '#b'
    | {d == 8}? '#o'
    | {d == 10}? (| '#d')
    | {d == 16}? '#x'
;

digitR[int d]
    : {d == 2}? ('0' | '1')
    | {d == 8}? ('0' | '1' | '2' | '3' | '4' | '5' | '6' | '7')
    | {d == 10}? Digit
    | {d == 16}? (Digit | 'a' | 'b' | 'c' | 'd' | 'e' | 'f')
;

/* External representations */

/* LEFT RECURSION */
datum
    : simpleDatum
    | compoundDatum
;

simpleDatum
    : boolean
    | number
    | character
    | string
    | symbol
;

symbol
    : identifier
;

/* LEFT RECURSION */
compoundDatum
    : listScheme
    | vector
;

/* LEFT RECURSION */
listScheme
    : '(' datum* ')'
    | '(' datum+ '.' datum ')'
    | abbreviation
;

/* LEFT RECURSION */
abbreviation
    : abbrevPrefix datum
;

abbrevPrefix
    : '\''
    | '`'
    | ','
    | ',@'
;

vector
    : '#(' datum* ')'
;

/* Expressions */

expression
    : variable
    | literal
    | procedureCall
    | lambdaExpression
    | conditional
    | assignment
    | derivedExpression
    | macroUse
    | macroBlock
;

literal
    : quotation
    | selfEvaluating
;

selfEvaluating
    : boolean
    | number
    | character
    | string
;

quotation
    : '\'' datum
    | '(' 'quote' datum ')'
;

procedureCall
    : '(' operator operand* ')'
;

operator
    : expression
;

operand
    : expression
;

lambdaExpression
    : '(' 'lambda' formals body ')'
;

formals
    : '(' variable* ')'
    | variable
    | '(' variable+ '.' variable ')'
;

body
    : definition* sequence
;

sequence
    : command* expression
;

command
    : expression
;

conditional
    : '(' 'if' test consequent alternate ')'
;

test
    : expression
;

consequent
    : expression
;

alternate
    : expression
    |
;

assignment
    : '(' 'set!' variable expression ')'
;

derivedExpression
    : '(' 'cond' condClause+ ')'
    | '(' 'cond' condClause* '(' 'else' sequence ')' ')'
    | '(' 'case' expression caseClause+ ')'
    | '(' 'case' expression caseClause* '(' 'else' sequence ')' ')'
    | '(' 'and' test* ')'
    | '(' 'or' test* ')'
    | '(' 'let' '(' bindingSpec* ')' body ')'
    | '(' 'let' variable '(' bindingSpec* ')' body ')'
    | '(' 'let*' '(' bindingSpec* ')' body ')'
    | '(' 'letrec' '(' bindingSpec* ')' body ')'
    | '(' 'begin' sequence ')'
    | '(' 'do' '(' iterationSpec* ')' '(' test doResult ')' command* ')'
    | '(' 'delay' expression ')'
    | quasiquotation
;

condClause
    : '(' test sequence ')'
    | '(' test ')'
    | '(' test '=>' recipient ')'
;

recipient
    : expression
;

caseClause
    : '(' '(' datum* ')' sequence ')'
;

bindingSpec
    : '(' variable expression ')'
;

iterationSpec
    : '(' variable init step ')'
    | '(' variable init ')'
;

init
    : expression
;

step
    : expression
;

doResult
    : sequence
    |
;

macroUse
    : '(' keyword datum* ')'
;

keyword
    : identifier
;

macroBlock
    : '(' 'let-syntax' '(' syntaxSpec* ')' body ')'
    | '(' 'letrec-syntax' '(' syntaxSpec* ')' body ')'
;

syntaxSpec
    : '(' keyword transformerSpec ')'
;

/* Quasiquotations */

quasiquotation
    : quasiquotationD[1]
;

quasiquotationD[int d]
    : '`' qqTemplateD[d]
    | '(' 'quasiquote' qqTemplateD[d] ')'
;

qqTemplateD[int d]
    : {d == 0}? expression
    | {d > 0}? (
          simpleDatum
        | listQQTemplateD[d]
        | vectorQQTemplateD[d]
        | unquotationD[d]
      )
;

listQQTemplateD[int d]
    : '(' qqTemplateOrSpliceD[d]* ')'
    | '(' qqTemplateOrSpliceD[d]+ '.' qqTemplateD[d] ')'
    | '\'' qqTemplateD[d]
    | quasiquotationD[d + 1]
;

vectorQQTemplateD[int d]
    : '#(' qqTemplateOrSpliceD[d]* ')'
;

unquotationD[int d]
    : ',' qqTemplateD[d - 1]
    | '(' 'unquote' qqTemplateD[d - 1] ')'
;

qqTemplateOrSpliceD[int d]
    : qqTemplateD[d]
    | splicingUnquotationD[d]
;

splicingUnquotationD[int d]
    : ',@' qqTemplateD[d - 1]
    | '(' 'unquote-splicing' qqTemplateD[d - 1] ')'
;

/* Transformers */

transformerSpec
    : '(' 'syntax-rules' '(' identifier* ')' syntaxRule* ')'
;

syntaxRule
    : '(' pattern template ')'
;

pattern
    : patternIdentifier
    | '(' pattern* ')'
    | '(' pattern+ '.' pattern ')'
    | '(' pattern* pattern ellipsis ')'
    | '#(' pattern* ')'
    | '#(' pattern* pattern ellipsis ')'
    | patternDatum
;

patternDatum
    : string
    | character
    | boolean
    | number
;

template
    : patternIdentifier
    | '(' templateElement* ')'
    | '(' templateElement+ '.' template ')'
    | '#(' templateElement* ')'
    | templateDatum
;

templateElement
    : template
    | template ellipsis
;

templateDatum
    : patternDatum
;

patternIdentifier
    : identifier
;

ellipsis
    : '...'
;

/* Programs and definitions */

program
    : commandOrDefinition*
;

commandOrDefinition
    : command
    | definition
    | syntaxDefinition
    | '(' 'begin' commandOrDefinition+ ')'
;

definition
    : '(' 'define' variable expression ')'
    | '(' 'define' '(' variable defFormals ')' body ')'
    | '(' 'begin' definition* ')'
;

defFormals
    : variable*
    | variable* '.' variable
;

syntaxDefinition
    : '(' 'define-syntax' keyword transformerSpec ')'
;
