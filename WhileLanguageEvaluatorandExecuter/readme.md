# HOW TO RUN CODE
You should use the following commands to run the parser :-  
```
make  
use "load-code.sml"; 
parseFile(filename(//in qoutes));  
```

For cleaning the parsing files
```
make clean
make  
use "load-code.sml"; 
parseFile(filename(//in qoutes));  
```  
for not using makefile
```
ml-lex while_AST.lex
ml-yacc while_AST.yacc
sml
use "load-code.sml";
parseFile(filename(as string));
```
to run the evaluator use runcode(filename)
The value stack will be empty if program is correct 
similar for control stack 
the memory stack shown on terminal is the final status of memory

# CONTEXT GRAMMAR USED (<N,T,P,S>)

## START SYBOL
START

## PRODUCTION RULES
PROGRAM => PROG ID COLON BLOCK  
BLOCK => DECLARATIONSEQ COMMANDSEQ  
DECLARATIONSEQ => DECLARATIONSEQ DECLARARTION | DECLARATION  
VTYPE => INT | BOOL  
VARIABLELIST => ID | VARIABLELIST COMMA ID   
COMMANDSEQ => BRACEL TEMPCOM BRACER | BRACEL BRACER  
TEMPCOM => TEMPCOM COMMAND TERM | COMMAND TERM  
COMMAND => ID SET EXP | READ ID | WRITE EXP | IF EXP THEN  COMMANDSEQ ENDIF | WHILE EXP DO COMMANDSEQ ENDWH  
EXP => EXP OP2 FAC | FAC  
FAC => NUM | ID | LEFTP EXP RIGHTP | TT | FF | NOT EXP | NEGATE EXP  
OP2 => PLUS | MINUS | TIMES | DIV | GREAT | GREATEQ | LESS | LESSEQ | EQUAL | AND | OR | BOOLSYMBOL | MOD  

## TERMINALS
LESS | GREAT | LESSEQ  | GREATEQ  | EQUAL  | BOOLSYMBOL | ADD  | SUB  | DIV  | MUL  | REM  | LEFTP | RIGHTP
| AND  | OR | TT  | FF  | ASSIGN| PROGT  | VAR | INT  | BOOL  | LISTYPE  | BRACEL  | BRACER  | COMMA  | READ  | WRITE
| WHILE  | DO  | ENDWH  | TERM  | NUM  | ID   | EOF | IF | ELSE | THEN | ENDIF | NEGATE | NOT | COLON

## NON TERMINALS
START  | PROGRAM | BLOCK  | DECLARATION | DECLARATIONSEQ | COMMAND  | COMMANDSEQ  | EXP 
| OP2  | VARIABLELIST  | TEMPCOM  | VTYPE | FAC 



# AST DATATYPE 
structure AST =
struct
datatype vartype = INT | BOOL 
datatype Program = PROG of  id * blk
and prog = PROGT
and assign = ASSIGN
and read = READ
and write = WRITE
and ifop = IF
and elseop = ELSE
and thenop = THEN
and endifop = ENDIF
and whileop = WHILE
and doop = DO
and endwhop = ENDWH
and blk = BLK1 of declsq * cmdsq | BLK2 of cmdsq
and doublcolon = COLON  
and declsq = declsq of declaration | Declsq11 of declsq * declaration
and declaration = declopop of var * varlist * listype * vartype * term
and term = TERM
and id = ID
and var = VAR
and listype = LISTYPE
and com = COMMA
and varlist = Varlist of id | Varlist2 of id * com * varlist
and cmdsq = Cmdsq1 of leftbrace * cmd * rightbrace | Cmdsq2 of leftbrace * rightbrace
and leftbrace = BRACEL
and rightbrace = BRACER
and ite = ITE
and wh = WH
and cmd =  Cmd2 of command * term | Cmd3 of cmd * command * term
and command = Command1 of id * assign * exp | Command2 of read * id | Command3 of write*exp | Command4 of ite * exp * cmdsq * cmdsq  | Command5 of wh * exp * cmdsq 

and op2 =  ADD | SUB | AND | OR | MUL | DIV | GREAT | GREATEQ | LESS | LESSEQ | EQUAL | REM | BOOLSYMBOL
and exp = Exp1 of exp*op2*fac | Exp2 of fac
and fac = Fac00 of num | Fac01 of id | Fac02 of tt | Fac03 of ff | Fac1 of leftp*exp*rightp | Fac2 of notop*exp | Fac3 of negate*exp
and num = NUM
and negate = NEGATE
and leftp = LEFTP
and rightp = RIGHTP
and notop = NOT  
and tt = TT
and ff = FF

end

# SYNTAX DIRECTED TRANSLATION


## SEMANTICS ACTION

START : PROGRAM (PROGRAM)  

PROGRAM : PROGT ID COLON BLOCK (AST.PROG( AST.ID, BLOCK)) 

BLOCK : DECLARATIONSEQ COMMANDSEQ (AST.BLK1(DECLARATIONSEQ, COMMANDSEQ)) | COMMANDSEQ(AST.BLK2(COMMANDSEQ)) 

DECLARATIONSEQ :  DECLARATIONSEQ DECLARATION (AST.Declsq11(DECLARATIONSEQ, DECLARATION)) | DECLARATION (AST.declsq(DECLARATION)) 

DECLARATION : VAR VARIABLELIST LISTYPE VTYPE TERM(AST.declopop(AST.VAR,VARIABLELIST,AST.LISTYPE,VTYPE,AST.TERM)) 

VTYPE : INT(AST.INT) | BOOL(AST.BOOL)

VARIABLELIST : ID  (AST.Varlist(AST.ID)) | VARIABLELIST COMMA ID (AST.Varlist2(AST.ID,AST.COMMA,VARIABLELIST)) 

COMMANDSEQ : BRACEL TEMPCOM BRACER(AST.Cmdsq1(AST.BRACEL,TEMPCOM,AST.BRACER)) | BRACEL BRACER(AST.Cmdsq2(AST.BRACEL,AST.BRACER))

TEMPCOM : TEMPCOM COMMAND TERM (AST.Cmd3(TEMPCOM,COMMAND,AST.TERM)) | COMMAND TERM (AST.Cmd2(COMMAND,AST.TERM)) 



COMMAND : ID ASSIGN EXP (AST.Command1(AST.ID,AST.ASSIGN,EXP)) | READ ID(AST.Command2(AST.READ,AST.ID)) | WRITE EXP(AST.Command3(AST.WRITE,EXP)) | IF EXP THEN COMMANDSEQ ELSE COMMANDSEQ ENDIF (AST.Command4(AST.ITE,EXP,COMMANDSEQ1,COMMANDSEQ2)) | WHILE EXP DO COMMANDSEQ ENDWH (AST.Command5(AST.WH,EXP,COMMANDSEQ)) 

EXP : EXP OP2 FAC (AST.Exp1(EXP,OP2,FAC)) | FAC (AST.Exp2(FAC))
FAC : NUM (AST.Fac00(AST.NUM)) | ID(AST.Fac01(AST.ID)) | LEFTP EXP RIGHTP(AST.Fac1(AST.LEFTP,EXP,AST.RIGHTP)) | TT(AST.Fac02(AST.TT)) | FF(AST.Fac03(AST.FF)) | NOT EXP(AST.Fac2(AST.NOT,EXP)) | NEGATE EXP (AST.Fac3(AST.NEGATE,EXP))

OP2 : ADD (AST.ADD) | SUB (AST.SUB ) | MUL(AST.MUL) | DIV(AST.DIV) | REM(AST.REM) |  GREAT(AST.GREAT) | GREATEQ(AST.GREATEQ) | LESS(AST.LESS) | LESSEQ(AST.LESSEQ) | EQUAL(AST.EQUAL) | AND (AST.AND) | OR (AST.OR) 



# OTHER DESIGNS
Some of my names differ from AST datatype mentioned in the Assignment pdf  
If you have to add 6 + 5 then please provide a gap between + and 5.

## HOW CAN Type Checking be implemented (TO BE USED IN NEXT PART
First of all define a hash table in which every identifier will be inserted as (ID.value,Type) and in command sequence we have to check whether this pair exist in a hash table.


# ACKNOWLEDGEMENTS 
For the loader.sml and load-code.sml, these are the standard code for running a parser. 





