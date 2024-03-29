exception Bad;
fun lookup s = 0                

%%
%name code

%term LESS | GREAT | LESSEQ  | GREATEQ  | EQUAL  | BOOLSYMBOL | PLUS | MINUS  | DIV  | MUL  | REM  | LEFTP | RIGHTP | NOT
| AND  | OR | TT | FF | ASSIGN| PROGT  | VAR | INT  | BOOL  | LISTYPE  | BRACEL  | BRACER  | COMMA  | READ  | WRITE
| WHILE  | DO  | ENDWH  | TERM  | NUM of int | ID of string  | EOF | IF | ELSE | THEN | ENDIF | NEGATE | COLON

%nonterm START of AST.command list * unit | PROGRAM of AST.command list * unit | BLOCK of AST.command list | DECLARATION of unit| DECLARATIONSEQ of unit| COMMAND of AST.command | COMMANDSEQ of AST.command list  
| OP2 of AST.op2 | VARIABLELIST of string list | TEMPCOM of AST.command list | VTYPE of AST.vartype | FAC of AST.fac 

%pos int  
%eop EOF
%noshift EOF


%left GREAT
 GREATEQ 
 LESS
 LESSEQ
 EQUAL
 AND OR
%left PLUS MINUS
%left MUL DIV REM
%right NEGATE NOT


%start START

%verbose

%%

START : PROGRAM (PROGRAM)  

PROGRAM : PROGT ID COLON BLOCK (BLOCK, AST.mem_idx := 0) 

BLOCK : DECLARATIONSEQ COMMANDSEQ (COMMANDSEQ) | COMMANDSEQ (COMMANDSEQ) 

DECLARATIONSEQ :  DECLARATIONSEQ DECLARATION    () | DECLARATION () 

DECLARATION : VAR VARIABLELIST LISTYPE VTYPE TERM(AST.pushtoTable(VARIABLELIST,VTYPE)) 

VTYPE : INT(AST.INT) | BOOL(AST.BOOL)

VARIABLELIST : ID  ([ID]) | VARIABLELIST COMMA ID (ID :: VARIABLELIST)

COMMANDSEQ : BRACEL TEMPCOM BRACER(TEMPCOM) | BRACEL BRACER([])

TEMPCOM : COMMAND TERM TEMPCOM (COMMAND::TEMPCOM) | COMMAND TERM ([COMMAND])

COMMAND : ID ASSIGN FAC (HashTable.lookup AST.symbolTable ID; AST.Command1(ID,FAC)) |
 READ ID(HashTable.lookup AST.symbolTable ID; AST.Command2(ID)) |
  WRITE FAC(AST.Command3(FAC)) |
   IF FAC THEN COMMANDSEQ ELSE COMMANDSEQ ENDIF (AST.Command4(FAC,COMMANDSEQ1,COMMANDSEQ2)) |
    WHILE FAC DO COMMANDSEQ ENDWH (AST.Command5(FAC,COMMANDSEQ)) 

FAC : NUM (AST.Fac00(NUM)) |
ID(HashTable.lookup AST.symbolTable  ID; AST.Fac01(ID)) |
LEFTP FAC RIGHTP(AST.Fac1(FAC)) | TT(AST.Fac02(true)) |
FF(AST.Fac02(false)) | NOT FAC(AST.Fac2(AST.NOT,FAC)) | 
NEGATE FAC (AST.Fac3(AST.NEGATE,FAC)) | 
FAC PLUS FAC (AST.Fac04(FAC1,AST.PLUS,FAC2))| 
FAC MINUS FAC(AST.Fac04(FAC1,AST.MINUS,FAC2)) | 
FAC MUL FAC(AST.Fac04(FAC1,AST.MUL,FAC2)) | 
FAC DIV FAC (AST.Fac04(FAC1,AST.DIV,FAC2))| 
FAC REM FAC(AST.Fac04(FAC1,AST.REM,FAC2)) | 
FAC GREAT FAC (AST.Fac04(FAC1,AST.GREAT,FAC2))| 
FAC GREATEQ FAC(AST.Fac04(FAC1,AST.GREATEQ,FAC2)) | 
FAC LESS FAC(AST.Fac04(FAC1,AST.LESS,FAC2)) |
FAC LESSEQ FAC(AST.Fac04(FAC1,AST.LESSEQ,FAC2)) | 
FAC EQUAL FAC (AST.Fac04(FAC1,AST.EQUAL,FAC2))| 
FAC AND FAC (AST.Fac04(FAC1,AST.AND,FAC2))| 
FAC OR FAC(AST.Fac04(FAC1,AST.OR,FAC2))
