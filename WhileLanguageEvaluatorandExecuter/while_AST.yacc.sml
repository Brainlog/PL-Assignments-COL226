functor codeLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : code_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
exception Bad;
fun lookup s = 0                


end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\054\000\002\000\053\000\003\000\052\000\004\000\051\000\
\\005\000\050\000\007\000\049\000\008\000\048\000\009\000\047\000\
\\010\000\046\000\011\000\045\000\013\000\077\000\015\000\044\000\
\\016\000\043\000\000\000\
\\001\000\001\000\054\000\002\000\053\000\003\000\052\000\004\000\051\000\
\\005\000\050\000\007\000\049\000\008\000\048\000\009\000\047\000\
\\010\000\046\000\011\000\045\000\015\000\044\000\016\000\043\000\
\\031\000\059\000\000\000\
\\001\000\001\000\054\000\002\000\053\000\003\000\052\000\004\000\051\000\
\\005\000\050\000\007\000\049\000\008\000\048\000\009\000\047\000\
\\010\000\046\000\011\000\045\000\015\000\044\000\016\000\043\000\
\\039\000\042\000\000\000\
\\001\000\012\000\034\000\014\000\033\000\017\000\032\000\018\000\031\000\
\\034\000\030\000\035\000\029\000\041\000\028\000\000\000\
\\001\000\019\000\035\000\000\000\
\\001\000\020\000\004\000\000\000\
\\001\000\021\000\012\000\025\000\011\000\000\000\
\\001\000\022\000\063\000\023\000\062\000\000\000\
\\001\000\024\000\040\000\027\000\039\000\000\000\
\\001\000\025\000\011\000\000\000\
\\001\000\026\000\022\000\028\000\021\000\029\000\020\000\030\000\019\000\
\\035\000\018\000\037\000\017\000\000\000\
\\001\000\026\000\025\000\000\000\
\\001\000\032\000\081\000\000\000\
\\001\000\033\000\026\000\000\000\
\\001\000\033\000\079\000\000\000\
\\001\000\035\000\005\000\000\000\
\\001\000\035\000\024\000\000\000\
\\001\000\035\000\038\000\000\000\
\\001\000\035\000\060\000\000\000\
\\001\000\036\000\000\000\000\000\
\\001\000\038\000\080\000\000\000\
\\001\000\040\000\083\000\000\000\
\\001\000\042\000\006\000\000\000\
\\085\000\000\000\
\\086\000\000\000\
\\087\000\000\000\
\\088\000\000\000\
\\089\000\000\000\
\\090\000\000\000\
\\091\000\000\000\
\\092\000\000\000\
\\093\000\000\000\
\\094\000\000\000\
\\095\000\000\000\
\\096\000\000\000\
\\097\000\000\000\
\\098\000\000\000\
\\099\000\028\000\021\000\029\000\020\000\030\000\019\000\035\000\018\000\
\\037\000\017\000\000\000\
\\100\000\001\000\054\000\002\000\053\000\003\000\052\000\004\000\051\000\
\\005\000\050\000\007\000\049\000\008\000\048\000\009\000\047\000\
\\010\000\046\000\011\000\045\000\015\000\044\000\016\000\043\000\000\000\
\\101\000\000\000\
\\102\000\001\000\054\000\002\000\053\000\003\000\052\000\004\000\051\000\
\\005\000\050\000\007\000\049\000\008\000\048\000\009\000\047\000\
\\010\000\046\000\011\000\045\000\015\000\044\000\016\000\043\000\000\000\
\\103\000\000\000\
\\104\000\000\000\
\\105\000\000\000\
\\106\000\000\000\
\\107\000\000\000\
\\108\000\000\000\
\\109\000\000\000\
\\110\000\000\000\
\\111\000\000\000\
\\112\000\009\000\047\000\010\000\046\000\011\000\045\000\000\000\
\\113\000\009\000\047\000\010\000\046\000\011\000\045\000\000\000\
\\114\000\000\000\
\\115\000\000\000\
\\116\000\000\000\
\\117\000\007\000\049\000\008\000\048\000\009\000\047\000\010\000\046\000\
\\011\000\045\000\000\000\
\\118\000\007\000\049\000\008\000\048\000\009\000\047\000\010\000\046\000\
\\011\000\045\000\000\000\
\\119\000\007\000\049\000\008\000\048\000\009\000\047\000\010\000\046\000\
\\011\000\045\000\000\000\
\\120\000\007\000\049\000\008\000\048\000\009\000\047\000\010\000\046\000\
\\011\000\045\000\000\000\
\\121\000\007\000\049\000\008\000\048\000\009\000\047\000\010\000\046\000\
\\011\000\045\000\000\000\
\\122\000\007\000\049\000\008\000\048\000\009\000\047\000\010\000\046\000\
\\011\000\045\000\000\000\
\\123\000\007\000\049\000\008\000\048\000\009\000\047\000\010\000\046\000\
\\011\000\045\000\000\000\
\"
val actionRowNumbers =
"\005\000\023\000\015\000\022\000\
\\006\000\026\000\006\000\028\000\
\\024\000\010\000\016\000\025\000\
\\027\000\011\000\013\000\003\000\
\\004\000\003\000\003\000\017\000\
\\035\000\008\000\032\000\034\000\
\\037\000\002\000\003\000\044\000\
\\043\000\047\000\046\000\003\000\
\\003\000\003\000\001\000\040\000\
\\039\000\018\000\007\000\036\000\
\\009\000\003\000\003\000\003\000\
\\003\000\003\000\003\000\003\000\
\\003\000\003\000\003\000\003\000\
\\003\000\049\000\048\000\000\000\
\\038\000\009\000\033\000\014\000\
\\031\000\030\000\020\000\061\000\
\\060\000\054\000\052\000\053\000\
\\051\000\050\000\059\000\056\000\
\\058\000\055\000\057\000\045\000\
\\012\000\029\000\009\000\042\000\
\\021\000\041\000\019\000"
val gotoT =
"\
\\001\000\082\000\002\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\008\000\004\000\007\000\005\000\006\000\007\000\005\000\000\000\
\\000\000\
\\004\000\012\000\007\000\011\000\000\000\
\\000\000\
\\000\000\
\\006\000\014\000\010\000\013\000\000\000\
\\009\000\021\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\012\000\025\000\000\000\
\\000\000\
\\012\000\034\000\000\000\
\\012\000\035\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\006\000\014\000\010\000\039\000\000\000\
\\000\000\
\\012\000\053\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\012\000\054\000\000\000\
\\012\000\055\000\000\000\
\\012\000\056\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\011\000\059\000\000\000\
\\000\000\
\\007\000\062\000\000\000\
\\012\000\063\000\000\000\
\\012\000\064\000\000\000\
\\012\000\065\000\000\000\
\\012\000\066\000\000\000\
\\012\000\067\000\000\000\
\\012\000\068\000\000\000\
\\012\000\069\000\000\000\
\\012\000\070\000\000\000\
\\012\000\071\000\000\000\
\\012\000\072\000\000\000\
\\012\000\073\000\000\000\
\\012\000\074\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\007\000\076\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\007\000\080\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 83
val numrules = 39
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle General.Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(List.map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | ID of unit ->  (string) | NUM of unit ->  (int)
 | FAC of unit ->  (AST.fac) | VTYPE of unit ->  (AST.vartype)
 | TEMPCOM of unit ->  (AST.command list)
 | VARIABLELIST of unit ->  (string list) | OP2 of unit ->  (AST.op2)
 | COMMANDSEQ of unit ->  (AST.command list)
 | COMMAND of unit ->  (AST.command)
 | DECLARATIONSEQ of unit ->  (unit) | DECLARATION of unit ->  (unit)
 | BLOCK of unit ->  (AST.command list)
 | PROGRAM of unit ->  (AST.command list*unit)
 | START of unit ->  (AST.command list*unit)
end
type svalue = MlyValue.svalue
type result = AST.command list*unit
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn (T 35) => true | _ => false
val showTerminal =
fn (T 0) => "LESS"
  | (T 1) => "GREAT"
  | (T 2) => "LESSEQ"
  | (T 3) => "GREATEQ"
  | (T 4) => "EQUAL"
  | (T 5) => "BOOLSYMBOL"
  | (T 6) => "PLUS"
  | (T 7) => "MINUS"
  | (T 8) => "DIV"
  | (T 9) => "MUL"
  | (T 10) => "REM"
  | (T 11) => "LEFTP"
  | (T 12) => "RIGHTP"
  | (T 13) => "NOT"
  | (T 14) => "AND"
  | (T 15) => "OR"
  | (T 16) => "TT"
  | (T 17) => "FF"
  | (T 18) => "ASSIGN"
  | (T 19) => "PROGT"
  | (T 20) => "VAR"
  | (T 21) => "INT"
  | (T 22) => "BOOL"
  | (T 23) => "LISTYPE"
  | (T 24) => "BRACEL"
  | (T 25) => "BRACER"
  | (T 26) => "COMMA"
  | (T 27) => "READ"
  | (T 28) => "WRITE"
  | (T 29) => "WHILE"
  | (T 30) => "DO"
  | (T 31) => "ENDWH"
  | (T 32) => "TERM"
  | (T 33) => "NUM"
  | (T 34) => "ID"
  | (T 35) => "EOF"
  | (T 36) => "IF"
  | (T 37) => "ELSE"
  | (T 38) => "THEN"
  | (T 39) => "ENDIF"
  | (T 40) => "NEGATE"
  | (T 41) => "COLON"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 41) $$ (T 40) $$ (T 39) $$ (T 38) $$ (T 37) $$ (T 36) $$ (T 35)
 $$ (T 32) $$ (T 31) $$ (T 30) $$ (T 29) $$ (T 28) $$ (T 27) $$ (T 26)
 $$ (T 25) $$ (T 24) $$ (T 23) $$ (T 22) $$ (T 21) $$ (T 20) $$ (T 19)
 $$ (T 18) $$ (T 17) $$ (T 16) $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12)
 $$ (T 11) $$ (T 10) $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 5) $$ 
(T 4) $$ (T 3) $$ (T 2) $$ (T 1) $$ (T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.PROGRAM PROGRAM1, PROGRAM1left, 
PROGRAM1right)) :: rest671)) => let val  result = MlyValue.START (fn _
 => let val  (PROGRAM as PROGRAM1) = PROGRAM1 ()
 in (PROGRAM)
end)
 in ( LrTable.NT 0, ( result, PROGRAM1left, PROGRAM1right), rest671)

end
|  ( 1, ( ( _, ( MlyValue.BLOCK BLOCK1, _, BLOCK1right)) :: _ :: ( _, 
( MlyValue.ID ID1, _, _)) :: ( _, ( _, PROGT1left, _)) :: rest671)) =>
 let val  result = MlyValue.PROGRAM (fn _ => let val  ID1 = ID1 ()
 val  (BLOCK as BLOCK1) = BLOCK1 ()
 in (BLOCK, AST.mem_idx := 0)
end)
 in ( LrTable.NT 1, ( result, PROGT1left, BLOCK1right), rest671)
end
|  ( 2, ( ( _, ( MlyValue.COMMANDSEQ COMMANDSEQ1, _, COMMANDSEQ1right)
) :: ( _, ( MlyValue.DECLARATIONSEQ DECLARATIONSEQ1, 
DECLARATIONSEQ1left, _)) :: rest671)) => let val  result = 
MlyValue.BLOCK (fn _ => let val  DECLARATIONSEQ1 = DECLARATIONSEQ1 ()
 val  (COMMANDSEQ as COMMANDSEQ1) = COMMANDSEQ1 ()
 in (COMMANDSEQ)
end)
 in ( LrTable.NT 2, ( result, DECLARATIONSEQ1left, COMMANDSEQ1right), 
rest671)
end
|  ( 3, ( ( _, ( MlyValue.COMMANDSEQ COMMANDSEQ1, COMMANDSEQ1left, 
COMMANDSEQ1right)) :: rest671)) => let val  result = MlyValue.BLOCK
 (fn _ => let val  (COMMANDSEQ as COMMANDSEQ1) = COMMANDSEQ1 ()
 in (COMMANDSEQ)
end)
 in ( LrTable.NT 2, ( result, COMMANDSEQ1left, COMMANDSEQ1right), 
rest671)
end
|  ( 4, ( ( _, ( MlyValue.DECLARATION DECLARATION1, _, 
DECLARATION1right)) :: ( _, ( MlyValue.DECLARATIONSEQ DECLARATIONSEQ1,
 DECLARATIONSEQ1left, _)) :: rest671)) => let val  result = 
MlyValue.DECLARATIONSEQ (fn _ => let val  DECLARATIONSEQ1 = 
DECLARATIONSEQ1 ()
 val  DECLARATION1 = DECLARATION1 ()
 in ()
end)
 in ( LrTable.NT 4, ( result, DECLARATIONSEQ1left, DECLARATION1right),
 rest671)
end
|  ( 5, ( ( _, ( MlyValue.DECLARATION DECLARATION1, DECLARATION1left, 
DECLARATION1right)) :: rest671)) => let val  result = 
MlyValue.DECLARATIONSEQ (fn _ => let val  DECLARATION1 = DECLARATION1
 ()
 in ()
end)
 in ( LrTable.NT 4, ( result, DECLARATION1left, DECLARATION1right), 
rest671)
end
|  ( 6, ( ( _, ( _, _, TERM1right)) :: ( _, ( MlyValue.VTYPE VTYPE1, _
, _)) :: _ :: ( _, ( MlyValue.VARIABLELIST VARIABLELIST1, _, _)) :: (
 _, ( _, VAR1left, _)) :: rest671)) => let val  result = 
MlyValue.DECLARATION (fn _ => let val  (VARIABLELIST as VARIABLELIST1)
 = VARIABLELIST1 ()
 val  (VTYPE as VTYPE1) = VTYPE1 ()
 in (AST.pushtoTable(VARIABLELIST,VTYPE))
end)
 in ( LrTable.NT 3, ( result, VAR1left, TERM1right), rest671)
end
|  ( 7, ( ( _, ( _, INT1left, INT1right)) :: rest671)) => let val  
result = MlyValue.VTYPE (fn _ => (AST.INT))
 in ( LrTable.NT 10, ( result, INT1left, INT1right), rest671)
end
|  ( 8, ( ( _, ( _, BOOL1left, BOOL1right)) :: rest671)) => let val  
result = MlyValue.VTYPE (fn _ => (AST.BOOL))
 in ( LrTable.NT 10, ( result, BOOL1left, BOOL1right), rest671)
end
|  ( 9, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) =>
 let val  result = MlyValue.VARIABLELIST (fn _ => let val  (ID as ID1)
 = ID1 ()
 in ([ID])
end)
 in ( LrTable.NT 8, ( result, ID1left, ID1right), rest671)
end
|  ( 10, ( ( _, ( MlyValue.ID ID1, _, ID1right)) :: _ :: ( _, ( 
MlyValue.VARIABLELIST VARIABLELIST1, VARIABLELIST1left, _)) :: rest671
)) => let val  result = MlyValue.VARIABLELIST (fn _ => let val  (
VARIABLELIST as VARIABLELIST1) = VARIABLELIST1 ()
 val  (ID as ID1) = ID1 ()
 in (ID :: VARIABLELIST)
end)
 in ( LrTable.NT 8, ( result, VARIABLELIST1left, ID1right), rest671)

end
|  ( 11, ( ( _, ( _, _, BRACER1right)) :: ( _, ( MlyValue.TEMPCOM 
TEMPCOM1, _, _)) :: ( _, ( _, BRACEL1left, _)) :: rest671)) => let
 val  result = MlyValue.COMMANDSEQ (fn _ => let val  (TEMPCOM as 
TEMPCOM1) = TEMPCOM1 ()
 in (TEMPCOM)
end)
 in ( LrTable.NT 6, ( result, BRACEL1left, BRACER1right), rest671)
end
|  ( 12, ( ( _, ( _, _, BRACER1right)) :: ( _, ( _, BRACEL1left, _))
 :: rest671)) => let val  result = MlyValue.COMMANDSEQ (fn _ => ([]))
 in ( LrTable.NT 6, ( result, BRACEL1left, BRACER1right), rest671)
end
|  ( 13, ( ( _, ( MlyValue.TEMPCOM TEMPCOM1, _, TEMPCOM1right)) :: _
 :: ( _, ( MlyValue.COMMAND COMMAND1, COMMAND1left, _)) :: rest671))
 => let val  result = MlyValue.TEMPCOM (fn _ => let val  (COMMAND as 
COMMAND1) = COMMAND1 ()
 val  (TEMPCOM as TEMPCOM1) = TEMPCOM1 ()
 in (COMMAND::TEMPCOM)
end)
 in ( LrTable.NT 9, ( result, COMMAND1left, TEMPCOM1right), rest671)

end
|  ( 14, ( ( _, ( _, _, TERM1right)) :: ( _, ( MlyValue.COMMAND 
COMMAND1, COMMAND1left, _)) :: rest671)) => let val  result = 
MlyValue.TEMPCOM (fn _ => let val  (COMMAND as COMMAND1) = COMMAND1 ()
 in ([COMMAND])
end)
 in ( LrTable.NT 9, ( result, COMMAND1left, TERM1right), rest671)
end
|  ( 15, ( ( _, ( MlyValue.FAC FAC1, _, FAC1right)) :: _ :: ( _, ( 
MlyValue.ID ID1, ID1left, _)) :: rest671)) => let val  result = 
MlyValue.COMMAND (fn _ => let val  (ID as ID1) = ID1 ()
 val  (FAC as FAC1) = FAC1 ()
 in (HashTable.lookup AST.symbolTable ID; AST.Command1(ID,FAC))
end)
 in ( LrTable.NT 5, ( result, ID1left, FAC1right), rest671)
end
|  ( 16, ( ( _, ( MlyValue.ID ID1, _, ID1right)) :: ( _, ( _, 
READ1left, _)) :: rest671)) => let val  result = MlyValue.COMMAND (fn
 _ => let val  (ID as ID1) = ID1 ()
 in (HashTable.lookup AST.symbolTable ID; AST.Command2(ID))
end)
 in ( LrTable.NT 5, ( result, READ1left, ID1right), rest671)
end
|  ( 17, ( ( _, ( MlyValue.FAC FAC1, _, FAC1right)) :: ( _, ( _, 
WRITE1left, _)) :: rest671)) => let val  result = MlyValue.COMMAND (fn
 _ => let val  (FAC as FAC1) = FAC1 ()
 in (AST.Command3(FAC))
end)
 in ( LrTable.NT 5, ( result, WRITE1left, FAC1right), rest671)
end
|  ( 18, ( ( _, ( _, _, ENDIF1right)) :: ( _, ( MlyValue.COMMANDSEQ 
COMMANDSEQ2, _, _)) :: _ :: ( _, ( MlyValue.COMMANDSEQ COMMANDSEQ1, _,
 _)) :: _ :: ( _, ( MlyValue.FAC FAC1, _, _)) :: ( _, ( _, IF1left, _)
) :: rest671)) => let val  result = MlyValue.COMMAND (fn _ => let val 
 (FAC as FAC1) = FAC1 ()
 val  COMMANDSEQ1 = COMMANDSEQ1 ()
 val  COMMANDSEQ2 = COMMANDSEQ2 ()
 in (AST.Command4(FAC,COMMANDSEQ1,COMMANDSEQ2))
end)
 in ( LrTable.NT 5, ( result, IF1left, ENDIF1right), rest671)
end
|  ( 19, ( ( _, ( _, _, ENDWH1right)) :: ( _, ( MlyValue.COMMANDSEQ 
COMMANDSEQ1, _, _)) :: _ :: ( _, ( MlyValue.FAC FAC1, _, _)) :: ( _, (
 _, WHILE1left, _)) :: rest671)) => let val  result = MlyValue.COMMAND
 (fn _ => let val  (FAC as FAC1) = FAC1 ()
 val  (COMMANDSEQ as COMMANDSEQ1) = COMMANDSEQ1 ()
 in (AST.Command5(FAC,COMMANDSEQ))
end)
 in ( LrTable.NT 5, ( result, WHILE1left, ENDWH1right), rest671)
end
|  ( 20, ( ( _, ( MlyValue.NUM NUM1, NUM1left, NUM1right)) :: rest671)
) => let val  result = MlyValue.FAC (fn _ => let val  (NUM as NUM1) = 
NUM1 ()
 in (AST.Fac00(NUM))
end)
 in ( LrTable.NT 11, ( result, NUM1left, NUM1right), rest671)
end
|  ( 21, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) =>
 let val  result = MlyValue.FAC (fn _ => let val  (ID as ID1) = ID1 ()
 in (HashTable.lookup AST.symbolTable  ID; AST.Fac01(ID))
end)
 in ( LrTable.NT 11, ( result, ID1left, ID1right), rest671)
end
|  ( 22, ( ( _, ( _, _, RIGHTP1right)) :: ( _, ( MlyValue.FAC FAC1, _,
 _)) :: ( _, ( _, LEFTP1left, _)) :: rest671)) => let val  result = 
MlyValue.FAC (fn _ => let val  (FAC as FAC1) = FAC1 ()
 in (AST.Fac1(FAC))
end)
 in ( LrTable.NT 11, ( result, LEFTP1left, RIGHTP1right), rest671)
end
|  ( 23, ( ( _, ( _, TT1left, TT1right)) :: rest671)) => let val  
result = MlyValue.FAC (fn _ => (AST.Fac02(true)))
 in ( LrTable.NT 11, ( result, TT1left, TT1right), rest671)
end
|  ( 24, ( ( _, ( _, FF1left, FF1right)) :: rest671)) => let val  
result = MlyValue.FAC (fn _ => (AST.Fac02(false)))
 in ( LrTable.NT 11, ( result, FF1left, FF1right), rest671)
end
|  ( 25, ( ( _, ( MlyValue.FAC FAC1, _, FAC1right)) :: ( _, ( _, 
NOT1left, _)) :: rest671)) => let val  result = MlyValue.FAC (fn _ =>
 let val  (FAC as FAC1) = FAC1 ()
 in (AST.Fac2(AST.NOT,FAC))
end)
 in ( LrTable.NT 11, ( result, NOT1left, FAC1right), rest671)
end
|  ( 26, ( ( _, ( MlyValue.FAC FAC1, _, FAC1right)) :: ( _, ( _, 
NEGATE1left, _)) :: rest671)) => let val  result = MlyValue.FAC (fn _
 => let val  (FAC as FAC1) = FAC1 ()
 in (AST.Fac3(AST.NEGATE,FAC))
end)
 in ( LrTable.NT 11, ( result, NEGATE1left, FAC1right), rest671)
end
|  ( 27, ( ( _, ( MlyValue.FAC FAC2, _, FAC2right)) :: _ :: ( _, ( 
MlyValue.FAC FAC1, FAC1left, _)) :: rest671)) => let val  result = 
MlyValue.FAC (fn _ => let val  FAC1 = FAC1 ()
 val  FAC2 = FAC2 ()
 in (AST.Fac04(FAC1,AST.PLUS,FAC2))
end)
 in ( LrTable.NT 11, ( result, FAC1left, FAC2right), rest671)
end
|  ( 28, ( ( _, ( MlyValue.FAC FAC2, _, FAC2right)) :: _ :: ( _, ( 
MlyValue.FAC FAC1, FAC1left, _)) :: rest671)) => let val  result = 
MlyValue.FAC (fn _ => let val  FAC1 = FAC1 ()
 val  FAC2 = FAC2 ()
 in (AST.Fac04(FAC1,AST.MINUS,FAC2))
end)
 in ( LrTable.NT 11, ( result, FAC1left, FAC2right), rest671)
end
|  ( 29, ( ( _, ( MlyValue.FAC FAC2, _, FAC2right)) :: _ :: ( _, ( 
MlyValue.FAC FAC1, FAC1left, _)) :: rest671)) => let val  result = 
MlyValue.FAC (fn _ => let val  FAC1 = FAC1 ()
 val  FAC2 = FAC2 ()
 in (AST.Fac04(FAC1,AST.MUL,FAC2))
end)
 in ( LrTable.NT 11, ( result, FAC1left, FAC2right), rest671)
end
|  ( 30, ( ( _, ( MlyValue.FAC FAC2, _, FAC2right)) :: _ :: ( _, ( 
MlyValue.FAC FAC1, FAC1left, _)) :: rest671)) => let val  result = 
MlyValue.FAC (fn _ => let val  FAC1 = FAC1 ()
 val  FAC2 = FAC2 ()
 in (AST.Fac04(FAC1,AST.DIV,FAC2))
end)
 in ( LrTable.NT 11, ( result, FAC1left, FAC2right), rest671)
end
|  ( 31, ( ( _, ( MlyValue.FAC FAC2, _, FAC2right)) :: _ :: ( _, ( 
MlyValue.FAC FAC1, FAC1left, _)) :: rest671)) => let val  result = 
MlyValue.FAC (fn _ => let val  FAC1 = FAC1 ()
 val  FAC2 = FAC2 ()
 in (AST.Fac04(FAC1,AST.REM,FAC2))
end)
 in ( LrTable.NT 11, ( result, FAC1left, FAC2right), rest671)
end
|  ( 32, ( ( _, ( MlyValue.FAC FAC2, _, FAC2right)) :: _ :: ( _, ( 
MlyValue.FAC FAC1, FAC1left, _)) :: rest671)) => let val  result = 
MlyValue.FAC (fn _ => let val  FAC1 = FAC1 ()
 val  FAC2 = FAC2 ()
 in (AST.Fac04(FAC1,AST.GREAT,FAC2))
end)
 in ( LrTable.NT 11, ( result, FAC1left, FAC2right), rest671)
end
|  ( 33, ( ( _, ( MlyValue.FAC FAC2, _, FAC2right)) :: _ :: ( _, ( 
MlyValue.FAC FAC1, FAC1left, _)) :: rest671)) => let val  result = 
MlyValue.FAC (fn _ => let val  FAC1 = FAC1 ()
 val  FAC2 = FAC2 ()
 in (AST.Fac04(FAC1,AST.GREATEQ,FAC2))
end)
 in ( LrTable.NT 11, ( result, FAC1left, FAC2right), rest671)
end
|  ( 34, ( ( _, ( MlyValue.FAC FAC2, _, FAC2right)) :: _ :: ( _, ( 
MlyValue.FAC FAC1, FAC1left, _)) :: rest671)) => let val  result = 
MlyValue.FAC (fn _ => let val  FAC1 = FAC1 ()
 val  FAC2 = FAC2 ()
 in (AST.Fac04(FAC1,AST.LESS,FAC2))
end)
 in ( LrTable.NT 11, ( result, FAC1left, FAC2right), rest671)
end
|  ( 35, ( ( _, ( MlyValue.FAC FAC2, _, FAC2right)) :: _ :: ( _, ( 
MlyValue.FAC FAC1, FAC1left, _)) :: rest671)) => let val  result = 
MlyValue.FAC (fn _ => let val  FAC1 = FAC1 ()
 val  FAC2 = FAC2 ()
 in (AST.Fac04(FAC1,AST.LESSEQ,FAC2))
end)
 in ( LrTable.NT 11, ( result, FAC1left, FAC2right), rest671)
end
|  ( 36, ( ( _, ( MlyValue.FAC FAC2, _, FAC2right)) :: _ :: ( _, ( 
MlyValue.FAC FAC1, FAC1left, _)) :: rest671)) => let val  result = 
MlyValue.FAC (fn _ => let val  FAC1 = FAC1 ()
 val  FAC2 = FAC2 ()
 in (AST.Fac04(FAC1,AST.EQUAL,FAC2))
end)
 in ( LrTable.NT 11, ( result, FAC1left, FAC2right), rest671)
end
|  ( 37, ( ( _, ( MlyValue.FAC FAC2, _, FAC2right)) :: _ :: ( _, ( 
MlyValue.FAC FAC1, FAC1left, _)) :: rest671)) => let val  result = 
MlyValue.FAC (fn _ => let val  FAC1 = FAC1 ()
 val  FAC2 = FAC2 ()
 in (AST.Fac04(FAC1,AST.AND,FAC2))
end)
 in ( LrTable.NT 11, ( result, FAC1left, FAC2right), rest671)
end
|  ( 38, ( ( _, ( MlyValue.FAC FAC2, _, FAC2right)) :: _ :: ( _, ( 
MlyValue.FAC FAC1, FAC1left, _)) :: rest671)) => let val  result = 
MlyValue.FAC (fn _ => let val  FAC1 = FAC1 ()
 val  FAC2 = FAC2 ()
 in (AST.Fac04(FAC1,AST.OR,FAC2))
end)
 in ( LrTable.NT 11, ( result, FAC1left, FAC2right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.START x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : code_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun LESS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VOID,p1,p2))
fun GREAT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.VOID,p1,p2))
fun LESSEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.VOID,p1,p2))
fun GREATEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun EQUAL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun BOOLSYMBOL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun MINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun DIV (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun MUL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun REM (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun LEFTP (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun RIGHTP (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun NOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun OR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun TT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun FF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun ASSIGN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun PROGT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun VAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun INT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun BOOL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun LISTYPE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun BRACEL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun BRACER (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.VOID,p1,p2))
fun READ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.VOID,p1,p2))
fun WRITE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.VOID,p1,p2))
fun WHILE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.VOID,p1,p2))
fun DO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.VOID,p1,p2))
fun ENDWH (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(
ParserData.MlyValue.VOID,p1,p2))
fun TERM (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(
ParserData.MlyValue.VOID,p1,p2))
fun NUM (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(
ParserData.MlyValue.NUM (fn () => i),p1,p2))
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 34,(
ParserData.MlyValue.ID (fn () => i),p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 35,(
ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 36,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 37,(
ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 38,(
ParserData.MlyValue.VOID,p1,p2))
fun ENDIF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 39,(
ParserData.MlyValue.VOID,p1,p2))
fun NEGATE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 40,(
ParserData.MlyValue.VOID,p1,p2))
fun COLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 41,(
ParserData.MlyValue.VOID,p1,p2))
end
end
