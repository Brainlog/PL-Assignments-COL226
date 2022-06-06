structure AST =
struct
type ID = string

datatype vartype = INT | BOOL 
datatype Program = PROG of  string * blk
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
and var = VAR
and listype = LISTYPE
and com = COMMA
and varlist = Varlist of string | Varlist2 of string * com * varlist
and cmdsq = Cmdsq1 of leftbrace * cmd * rightbrace | Cmdsq2 of leftbrace * rightbrace
and leftbrace = BRACEL
and rightbrace = BRACER
and ite = ITE
and wh = WH
and cmd =  Cmd of command list | Cmd1 of command
and command = Command1 of string * fac | Command2 of string | Command3 of fac | Command4 of  fac * command list * command list  | Command5 of fac * command list 

and op2 =  PLUS | MINUS | AND | OR | MUL | DIV |  GREAT | GREATEQ | LESS | LESSEQ | EQUAL | REM | BOOLSYMBOL

and fac = Fac00 of int | Fac01 of string | Fac02 of bool  | Fac1 of fac | Fac2 of notop*fac | Fac3 of negate*fac| Fac04 of fac*op2*fac 

and negate = NEGATE
and leftp = LEFTP
and rightp = RIGHTP
and notop = NOT  
and tt = TT
and ff = FF
and value = Intv of int | Boolv  of bool
 val symbolTable:(string,(vartype * int)) HashTable.hash_table=HashTable.mkTable(HashString.hashString,op=)(100, Fail "Not Found");
    val mem_idx=ref 0;
    fun pushtoTable([],ty:vartype)=()
    |pushtoTable(x::xs,ty:vartype)= (HashTable.insert symbolTable (x,(ty,!mem_idx)); mem_idx:=(!mem_idx)+1;
     pushtoTable(xs,ty)
     )
     val it = print(Int.toString(!mem_idx));
    fun fk (a) = HashTable.insert symbolTable (a,(INT,!mem_idx)); 
    
end




