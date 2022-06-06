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