signature code_TOKENS =
sig
type ('a,'b) token
type svalue
val COLON:  'a * 'a -> (svalue,'a) token
val NEGATE:  'a * 'a -> (svalue,'a) token
val ENDIF:  'a * 'a -> (svalue,'a) token
val THEN:  'a * 'a -> (svalue,'a) token
val ELSE:  'a * 'a -> (svalue,'a) token
val IF:  'a * 'a -> (svalue,'a) token
val EOF:  'a * 'a -> (svalue,'a) token
val ID: (string) *  'a * 'a -> (svalue,'a) token
val NUM: (int) *  'a * 'a -> (svalue,'a) token
val TERM:  'a * 'a -> (svalue,'a) token
val ENDWH:  'a * 'a -> (svalue,'a) token
val DO:  'a * 'a -> (svalue,'a) token
val WHILE:  'a * 'a -> (svalue,'a) token
val WRITE:  'a * 'a -> (svalue,'a) token
val READ:  'a * 'a -> (svalue,'a) token
val COMMA:  'a * 'a -> (svalue,'a) token
val BRACER:  'a * 'a -> (svalue,'a) token
val BRACEL:  'a * 'a -> (svalue,'a) token
val LISTYPE:  'a * 'a -> (svalue,'a) token
val BOOL:  'a * 'a -> (svalue,'a) token
val INT:  'a * 'a -> (svalue,'a) token
val VAR:  'a * 'a -> (svalue,'a) token
val PROGT:  'a * 'a -> (svalue,'a) token
val ASSIGN:  'a * 'a -> (svalue,'a) token
val FF:  'a * 'a -> (svalue,'a) token
val TT:  'a * 'a -> (svalue,'a) token
val OR:  'a * 'a -> (svalue,'a) token
val AND:  'a * 'a -> (svalue,'a) token
val NOT:  'a * 'a -> (svalue,'a) token
val RIGHTP:  'a * 'a -> (svalue,'a) token
val LEFTP:  'a * 'a -> (svalue,'a) token
val REM:  'a * 'a -> (svalue,'a) token
val MUL:  'a * 'a -> (svalue,'a) token
val DIV:  'a * 'a -> (svalue,'a) token
val MINUS:  'a * 'a -> (svalue,'a) token
val PLUS:  'a * 'a -> (svalue,'a) token
val BOOLSYMBOL:  'a * 'a -> (svalue,'a) token
val EQUAL:  'a * 'a -> (svalue,'a) token
val GREATEQ:  'a * 'a -> (svalue,'a) token
val LESSEQ:  'a * 'a -> (svalue,'a) token
val GREAT:  'a * 'a -> (svalue,'a) token
val LESS:  'a * 'a -> (svalue,'a) token
end
signature code_LRVALS=
sig
structure Tokens : code_TOKENS
structure ParserData:PARSER_DATA
sharing type ParserData.Token.token = Tokens.token
sharing type ParserData.svalue = Tokens.svalue
end
