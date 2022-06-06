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
fun lookup s = 0


end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\057\000\002\000\056\000\003\000\055\000\004\000\054\000\
\\005\000\053\000\007\000\052\000\008\000\051\000\009\000\050\000\
\\010\000\049\000\011\000\048\000\013\000\069\000\014\000\047\000\
\\015\000\046\000\000\000\
\\001\000\001\000\057\000\002\000\056\000\003\000\055\000\004\000\054\000\
\\005\000\053\000\007\000\052\000\008\000\051\000\009\000\050\000\
\\010\000\049\000\011\000\048\000\014\000\047\000\015\000\046\000\
\\030\000\062\000\000\000\
\\001\000\001\000\057\000\002\000\056\000\003\000\055\000\004\000\054\000\
\\005\000\053\000\007\000\052\000\008\000\051\000\009\000\050\000\
\\010\000\049\000\011\000\048\000\014\000\047\000\015\000\046\000\
\\038\000\045\000\000\000\
\\001\000\012\000\036\000\016\000\035\000\017\000\034\000\033\000\033\000\
\\034\000\032\000\040\000\031\000\041\000\030\000\000\000\
\\001\000\018\000\037\000\000\000\
\\001\000\019\000\004\000\000\000\
\\001\000\020\000\012\000\024\000\011\000\000\000\
\\001\000\021\000\066\000\022\000\065\000\000\000\
\\001\000\023\000\042\000\026\000\041\000\000\000\
\\001\000\024\000\011\000\000\000\
\\001\000\025\000\022\000\027\000\021\000\028\000\020\000\029\000\019\000\
\\034\000\018\000\036\000\017\000\000\000\
\\001\000\025\000\026\000\027\000\021\000\028\000\020\000\029\000\019\000\
\\034\000\018\000\036\000\017\000\000\000\
\\001\000\031\000\073\000\000\000\
\\001\000\032\000\027\000\000\000\
\\001\000\032\000\043\000\000\000\
\\001\000\032\000\071\000\000\000\
\\001\000\034\000\005\000\000\000\
\\001\000\034\000\024\000\000\000\
\\001\000\034\000\040\000\000\000\
\\001\000\034\000\063\000\000\000\
\\001\000\035\000\000\000\000\000\
\\001\000\037\000\072\000\000\000\
\\001\000\039\000\075\000\000\000\
\\001\000\042\000\006\000\000\000\
\\077\000\000\000\
\\078\000\000\000\
\\079\000\000\000\
\\080\000\000\000\
\\081\000\000\000\
\\082\000\000\000\
\\083\000\000\000\
\\084\000\000\000\
\\085\000\000\000\
\\086\000\000\000\
\\087\000\000\000\
\\088\000\000\000\
\\089\000\000\000\
\\090\000\000\000\
\\091\000\000\000\
\\092\000\001\000\057\000\002\000\056\000\003\000\055\000\004\000\054\000\
\\005\000\053\000\007\000\052\000\008\000\051\000\009\000\050\000\
\\010\000\049\000\011\000\048\000\014\000\047\000\015\000\046\000\000\000\
\\093\000\000\000\
\\094\000\001\000\057\000\002\000\056\000\003\000\055\000\004\000\054\000\
\\005\000\053\000\007\000\052\000\008\000\051\000\009\000\050\000\
\\010\000\049\000\011\000\048\000\014\000\047\000\015\000\046\000\000\000\
\\095\000\000\000\
\\096\000\000\000\
\\097\000\000\000\
\\098\000\000\000\
\\099\000\000\000\
\\100\000\000\000\
\\101\000\000\000\
\\102\000\000\000\
\\103\000\000\000\
\\104\000\000\000\
\\105\000\000\000\
\\106\000\000\000\
\\107\000\000\000\
\\108\000\000\000\
\\109\000\000\000\
\\110\000\000\000\
\\111\000\000\000\
\\112\000\000\000\
\\113\000\000\000\
\\114\000\000\000\
\\115\000\000\000\
\\116\000\000\000\
\\117\000\000\000\
\"
val actionRowNumbers =
"\005\000\024\000\016\000\023\000\
\\006\000\027\000\006\000\029\000\
\\025\000\010\000\017\000\026\000\
\\028\000\011\000\013\000\003\000\
\\004\000\003\000\003\000\018\000\
\\036\000\008\000\033\000\014\000\
\\035\000\038\000\045\000\002\000\
\\003\000\003\000\047\000\046\000\
\\050\000\049\000\003\000\003\000\
\\001\000\041\000\040\000\019\000\
\\007\000\037\000\003\000\009\000\
\\064\000\063\000\057\000\055\000\
\\056\000\054\000\053\000\062\000\
\\059\000\061\000\058\000\060\000\
\\051\000\052\000\000\000\039\000\
\\009\000\034\000\015\000\032\000\
\\031\000\044\000\021\000\048\000\
\\012\000\030\000\009\000\043\000\
\\022\000\042\000\020\000"
val gotoT =
"\
\\001\000\074\000\002\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\008\000\004\000\007\000\005\000\006\000\007\000\005\000\000\000\
\\000\000\
\\004\000\012\000\007\000\011\000\000\000\
\\000\000\
\\000\000\
\\006\000\014\000\011\000\013\000\000\000\
\\010\000\021\000\000\000\
\\000\000\
\\000\000\
\\006\000\023\000\000\000\
\\000\000\
\\008\000\027\000\013\000\026\000\000\000\
\\000\000\
\\008\000\036\000\013\000\026\000\000\000\
\\008\000\037\000\013\000\026\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\009\000\042\000\000\000\
\\008\000\056\000\013\000\026\000\000\000\
\\008\000\057\000\013\000\026\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\008\000\058\000\013\000\026\000\000\000\
\\008\000\059\000\013\000\026\000\000\000\
\\009\000\042\000\000\000\
\\009\000\042\000\000\000\
\\000\000\
\\000\000\
\\012\000\062\000\000\000\
\\000\000\
\\013\000\065\000\000\000\
\\007\000\066\000\000\000\
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
\\009\000\042\000\000\000\
\\009\000\042\000\000\000\
\\009\000\042\000\000\000\
\\009\000\042\000\000\000\
\\007\000\068\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\007\000\072\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 75
val numrules = 41
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
 | TEMPCOM of unit ->  (AST.cmd)
 | VARIABLELIST of unit ->  (AST.varlist) | OP2 of unit ->  (AST.op2)
 | EXP of unit ->  (AST.exp) | COMMANDSEQ of unit ->  (AST.cmdsq)
 | COMMAND of unit ->  (AST.command)
 | DECLARATIONSEQ of unit ->  (AST.declsq)
 | DECLARATION of unit ->  (AST.declaration)
 | BLOCK of unit ->  (AST.blk) | PROGRAM of unit ->  (AST.Program)
 | START of unit ->  (AST.Program)
end
type svalue = MlyValue.svalue
type result = AST.Program
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
fn (T 34) => true | _ => false
val showTerminal =
fn (T 0) => "LESS"
  | (T 1) => "GREAT"
  | (T 2) => "LESSEQ"
  | (T 3) => "GREATEQ"
  | (T 4) => "EQUAL"
  | (T 5) => "BOOLSYMBOL"
  | (T 6) => "ADD"
  | (T 7) => "SUB"
  | (T 8) => "DIV"
  | (T 9) => "MUL"
  | (T 10) => "REM"
  | (T 11) => "LEFTP"
  | (T 12) => "RIGHTP"
  | (T 13) => "AND"
  | (T 14) => "OR"
  | (T 15) => "TT"
  | (T 16) => "FF"
  | (T 17) => "ASSIGN"
  | (T 18) => "PROGT"
  | (T 19) => "VAR"
  | (T 20) => "INT"
  | (T 21) => "BOOL"
  | (T 22) => "LISTYPE"
  | (T 23) => "BRACEL"
  | (T 24) => "BRACER"
  | (T 25) => "COMMA"
  | (T 26) => "READ"
  | (T 27) => "WRITE"
  | (T 28) => "WHILE"
  | (T 29) => "DO"
  | (T 30) => "ENDWH"
  | (T 31) => "TERM"
  | (T 32) => "NUM"
  | (T 33) => "ID"
  | (T 34) => "EOF"
  | (T 35) => "IF"
  | (T 36) => "ELSE"
  | (T 37) => "THEN"
  | (T 38) => "ENDIF"
  | (T 39) => "NEGATE"
  | (T 40) => "NOT"
  | (T 41) => "COLON"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 41) $$ (T 40) $$ (T 39) $$ (T 38) $$ (T 37) $$ (T 36) $$ (T 35)
 $$ (T 34) $$ (T 31) $$ (T 30) $$ (T 29) $$ (T 28) $$ (T 27) $$ (T 26)
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
 let val  result = MlyValue.PROGRAM (fn _ => let val  (ID as ID1) = 
ID1 ()
 val  (BLOCK as BLOCK1) = BLOCK1 ()
 in (AST.PROG( AST.ID, BLOCK))
end)
 in ( LrTable.NT 1, ( result, PROGT1left, BLOCK1right), rest671)
end
|  ( 2, ( ( _, ( MlyValue.COMMANDSEQ COMMANDSEQ1, _, COMMANDSEQ1right)
) :: ( _, ( MlyValue.DECLARATIONSEQ DECLARATIONSEQ1, 
DECLARATIONSEQ1left, _)) :: rest671)) => let val  result = 
MlyValue.BLOCK (fn _ => let val  (DECLARATIONSEQ as DECLARATIONSEQ1) =
 DECLARATIONSEQ1 ()
 val  (COMMANDSEQ as COMMANDSEQ1) = COMMANDSEQ1 ()
 in (AST.BLK1(DECLARATIONSEQ, COMMANDSEQ))
end)
 in ( LrTable.NT 2, ( result, DECLARATIONSEQ1left, COMMANDSEQ1right), 
rest671)
end
|  ( 3, ( ( _, ( MlyValue.COMMANDSEQ COMMANDSEQ1, COMMANDSEQ1left, 
COMMANDSEQ1right)) :: rest671)) => let val  result = MlyValue.BLOCK
 (fn _ => let val  (COMMANDSEQ as COMMANDSEQ1) = COMMANDSEQ1 ()
 in (AST.BLK2(COMMANDSEQ))
end)
 in ( LrTable.NT 2, ( result, COMMANDSEQ1left, COMMANDSEQ1right), 
rest671)
end
|  ( 4, ( ( _, ( MlyValue.DECLARATION DECLARATION1, _, 
DECLARATION1right)) :: ( _, ( MlyValue.DECLARATIONSEQ DECLARATIONSEQ1,
 DECLARATIONSEQ1left, _)) :: rest671)) => let val  result = 
MlyValue.DECLARATIONSEQ (fn _ => let val  (DECLARATIONSEQ as 
DECLARATIONSEQ1) = DECLARATIONSEQ1 ()
 val  (DECLARATION as DECLARATION1) = DECLARATION1 ()
 in (AST.Declsq11(DECLARATIONSEQ, DECLARATION))
end)
 in ( LrTable.NT 4, ( result, DECLARATIONSEQ1left, DECLARATION1right),
 rest671)
end
|  ( 5, ( ( _, ( MlyValue.DECLARATION DECLARATION1, DECLARATION1left, 
DECLARATION1right)) :: rest671)) => let val  result = 
MlyValue.DECLARATIONSEQ (fn _ => let val  (DECLARATION as DECLARATION1
) = DECLARATION1 ()
 in (AST.declsq(DECLARATION))
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
 in (AST.declopop(AST.VAR,VARIABLELIST,AST.LISTYPE,VTYPE,AST.TERM))

end)
 in ( LrTable.NT 3, ( result, VAR1left, TERM1right), rest671)
end
|  ( 7, ( ( _, ( _, INT1left, INT1right)) :: rest671)) => let val  
result = MlyValue.VTYPE (fn _ => (AST.INT))
 in ( LrTable.NT 11, ( result, INT1left, INT1right), rest671)
end
|  ( 8, ( ( _, ( _, BOOL1left, BOOL1right)) :: rest671)) => let val  
result = MlyValue.VTYPE (fn _ => (AST.BOOL))
 in ( LrTable.NT 11, ( result, BOOL1left, BOOL1right), rest671)
end
|  ( 9, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) =>
 let val  result = MlyValue.VARIABLELIST (fn _ => let val  (ID as ID1)
 = ID1 ()
 in (AST.Varlist(AST.ID))
end)
 in ( LrTable.NT 9, ( result, ID1left, ID1right), rest671)
end
|  ( 10, ( ( _, ( MlyValue.ID ID1, _, ID1right)) :: _ :: ( _, ( 
MlyValue.VARIABLELIST VARIABLELIST1, VARIABLELIST1left, _)) :: rest671
)) => let val  result = MlyValue.VARIABLELIST (fn _ => let val  (
VARIABLELIST as VARIABLELIST1) = VARIABLELIST1 ()
 val  (ID as ID1) = ID1 ()
 in (AST.Varlist2(AST.ID,AST.COMMA,VARIABLELIST))
end)
 in ( LrTable.NT 9, ( result, VARIABLELIST1left, ID1right), rest671)

end
|  ( 11, ( ( _, ( _, _, BRACER1right)) :: ( _, ( MlyValue.TEMPCOM 
TEMPCOM1, _, _)) :: ( _, ( _, BRACEL1left, _)) :: rest671)) => let
 val  result = MlyValue.COMMANDSEQ (fn _ => let val  (TEMPCOM as 
TEMPCOM1) = TEMPCOM1 ()
 in (AST.Cmdsq1(AST.BRACEL,TEMPCOM,AST.BRACER))
end)
 in ( LrTable.NT 6, ( result, BRACEL1left, BRACER1right), rest671)
end
|  ( 12, ( ( _, ( _, _, BRACER1right)) :: ( _, ( _, BRACEL1left, _))
 :: rest671)) => let val  result = MlyValue.COMMANDSEQ (fn _ => (
AST.Cmdsq2(AST.BRACEL,AST.BRACER)))
 in ( LrTable.NT 6, ( result, BRACEL1left, BRACER1right), rest671)
end
|  ( 13, ( ( _, ( _, _, TERM1right)) :: ( _, ( MlyValue.COMMAND 
COMMAND1, _, _)) :: ( _, ( MlyValue.TEMPCOM TEMPCOM1, TEMPCOM1left, _)
) :: rest671)) => let val  result = MlyValue.TEMPCOM (fn _ => let val 
 (TEMPCOM as TEMPCOM1) = TEMPCOM1 ()
 val  (COMMAND as COMMAND1) = COMMAND1 ()
 in (AST.Cmd3(TEMPCOM,COMMAND,AST.TERM))
end)
 in ( LrTable.NT 10, ( result, TEMPCOM1left, TERM1right), rest671)
end
|  ( 14, ( ( _, ( _, _, TERM1right)) :: ( _, ( MlyValue.COMMAND 
COMMAND1, COMMAND1left, _)) :: rest671)) => let val  result = 
MlyValue.TEMPCOM (fn _ => let val  (COMMAND as COMMAND1) = COMMAND1 ()
 in (AST.Cmd2(COMMAND,AST.TERM))
end)
 in ( LrTable.NT 10, ( result, COMMAND1left, TERM1right), rest671)
end
|  ( 15, ( ( _, ( MlyValue.EXP EXP1, _, EXP1right)) :: _ :: ( _, ( 
MlyValue.ID ID1, ID1left, _)) :: rest671)) => let val  result = 
MlyValue.COMMAND (fn _ => let val  (ID as ID1) = ID1 ()
 val  (EXP as EXP1) = EXP1 ()
 in (AST.Command1(AST.ID,AST.ASSIGN,EXP))
end)
 in ( LrTable.NT 5, ( result, ID1left, EXP1right), rest671)
end
|  ( 16, ( ( _, ( MlyValue.ID ID1, _, ID1right)) :: ( _, ( _, 
READ1left, _)) :: rest671)) => let val  result = MlyValue.COMMAND (fn
 _ => let val  (ID as ID1) = ID1 ()
 in (AST.Command2(AST.READ,AST.ID))
end)
 in ( LrTable.NT 5, ( result, READ1left, ID1right), rest671)
end
|  ( 17, ( ( _, ( MlyValue.EXP EXP1, _, EXP1right)) :: ( _, ( _, 
WRITE1left, _)) :: rest671)) => let val  result = MlyValue.COMMAND (fn
 _ => let val  (EXP as EXP1) = EXP1 ()
 in (AST.Command3(AST.WRITE,EXP))
end)
 in ( LrTable.NT 5, ( result, WRITE1left, EXP1right), rest671)
end
|  ( 18, ( ( _, ( _, _, ENDIF1right)) :: ( _, ( MlyValue.COMMANDSEQ 
COMMANDSEQ2, _, _)) :: _ :: ( _, ( MlyValue.COMMANDSEQ COMMANDSEQ1, _,
 _)) :: _ :: ( _, ( MlyValue.EXP EXP1, _, _)) :: ( _, ( _, IF1left, _)
) :: rest671)) => let val  result = MlyValue.COMMAND (fn _ => let val 
 (EXP as EXP1) = EXP1 ()
 val  COMMANDSEQ1 = COMMANDSEQ1 ()
 val  COMMANDSEQ2 = COMMANDSEQ2 ()
 in (AST.Command4(AST.ITE,EXP,COMMANDSEQ1,COMMANDSEQ2))
end)
 in ( LrTable.NT 5, ( result, IF1left, ENDIF1right), rest671)
end
|  ( 19, ( ( _, ( _, _, ENDWH1right)) :: ( _, ( MlyValue.COMMANDSEQ 
COMMANDSEQ1, _, _)) :: _ :: ( _, ( MlyValue.EXP EXP1, _, _)) :: ( _, (
 _, WHILE1left, _)) :: rest671)) => let val  result = MlyValue.COMMAND
 (fn _ => let val  (EXP as EXP1) = EXP1 ()
 val  (COMMANDSEQ as COMMANDSEQ1) = COMMANDSEQ1 ()
 in (AST.Command5(AST.WH,EXP,COMMANDSEQ))
end)
 in ( LrTable.NT 5, ( result, WHILE1left, ENDWH1right), rest671)
end
|  ( 20, ( ( _, ( MlyValue.FAC FAC1, _, FAC1right)) :: ( _, ( 
MlyValue.OP2 OP21, _, _)) :: ( _, ( MlyValue.EXP EXP1, EXP1left, _))
 :: rest671)) => let val  result = MlyValue.EXP (fn _ => let val  (EXP
 as EXP1) = EXP1 ()
 val  (OP2 as OP21) = OP21 ()
 val  (FAC as FAC1) = FAC1 ()
 in (AST.Exp1(EXP,OP2,FAC))
end)
 in ( LrTable.NT 7, ( result, EXP1left, FAC1right), rest671)
end
|  ( 21, ( ( _, ( MlyValue.FAC FAC1, FAC1left, FAC1right)) :: rest671)
) => let val  result = MlyValue.EXP (fn _ => let val  (FAC as FAC1) = 
FAC1 ()
 in (AST.Exp2(FAC))
end)
 in ( LrTable.NT 7, ( result, FAC1left, FAC1right), rest671)
end
|  ( 22, ( ( _, ( MlyValue.NUM NUM1, NUM1left, NUM1right)) :: rest671)
) => let val  result = MlyValue.FAC (fn _ => let val  (NUM as NUM1) = 
NUM1 ()
 in (AST.Fac00(AST.NUM))
end)
 in ( LrTable.NT 12, ( result, NUM1left, NUM1right), rest671)
end
|  ( 23, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) =>
 let val  result = MlyValue.FAC (fn _ => let val  (ID as ID1) = ID1 ()
 in (AST.Fac01(AST.ID))
end)
 in ( LrTable.NT 12, ( result, ID1left, ID1right), rest671)
end
|  ( 24, ( ( _, ( _, _, RIGHTP1right)) :: ( _, ( MlyValue.EXP EXP1, _,
 _)) :: ( _, ( _, LEFTP1left, _)) :: rest671)) => let val  result = 
MlyValue.FAC (fn _ => let val  (EXP as EXP1) = EXP1 ()
 in (AST.Fac1(AST.LEFTP,EXP,AST.RIGHTP))
end)
 in ( LrTable.NT 12, ( result, LEFTP1left, RIGHTP1right), rest671)
end
|  ( 25, ( ( _, ( _, TT1left, TT1right)) :: rest671)) => let val  
result = MlyValue.FAC (fn _ => (AST.Fac02(AST.TT)))
 in ( LrTable.NT 12, ( result, TT1left, TT1right), rest671)
end
|  ( 26, ( ( _, ( _, FF1left, FF1right)) :: rest671)) => let val  
result = MlyValue.FAC (fn _ => (AST.Fac03(AST.FF)))
 in ( LrTable.NT 12, ( result, FF1left, FF1right), rest671)
end
|  ( 27, ( ( _, ( MlyValue.EXP EXP1, _, EXP1right)) :: ( _, ( _, 
NOT1left, _)) :: rest671)) => let val  result = MlyValue.FAC (fn _ =>
 let val  (EXP as EXP1) = EXP1 ()
 in (AST.Fac2(AST.NOT,EXP))
end)
 in ( LrTable.NT 12, ( result, NOT1left, EXP1right), rest671)
end
|  ( 28, ( ( _, ( MlyValue.EXP EXP1, _, EXP1right)) :: ( _, ( _, 
NEGATE1left, _)) :: rest671)) => let val  result = MlyValue.FAC (fn _
 => let val  (EXP as EXP1) = EXP1 ()
 in (AST.Fac3(AST.NEGATE,EXP))
end)
 in ( LrTable.NT 12, ( result, NEGATE1left, EXP1right), rest671)
end
|  ( 29, ( ( _, ( _, ADD1left, ADD1right)) :: rest671)) => let val  
result = MlyValue.OP2 (fn _ => (AST.ADD))
 in ( LrTable.NT 8, ( result, ADD1left, ADD1right), rest671)
end
|  ( 30, ( ( _, ( _, SUB1left, SUB1right)) :: rest671)) => let val  
result = MlyValue.OP2 (fn _ => (AST.SUB ))
 in ( LrTable.NT 8, ( result, SUB1left, SUB1right), rest671)
end
|  ( 31, ( ( _, ( _, MUL1left, MUL1right)) :: rest671)) => let val  
result = MlyValue.OP2 (fn _ => (AST.MUL))
 in ( LrTable.NT 8, ( result, MUL1left, MUL1right), rest671)
end
|  ( 32, ( ( _, ( _, DIV1left, DIV1right)) :: rest671)) => let val  
result = MlyValue.OP2 (fn _ => (AST.DIV))
 in ( LrTable.NT 8, ( result, DIV1left, DIV1right), rest671)
end
|  ( 33, ( ( _, ( _, REM1left, REM1right)) :: rest671)) => let val  
result = MlyValue.OP2 (fn _ => (AST.REM))
 in ( LrTable.NT 8, ( result, REM1left, REM1right), rest671)
end
|  ( 34, ( ( _, ( _, GREAT1left, GREAT1right)) :: rest671)) => let
 val  result = MlyValue.OP2 (fn _ => (AST.GREAT))
 in ( LrTable.NT 8, ( result, GREAT1left, GREAT1right), rest671)
end
|  ( 35, ( ( _, ( _, GREATEQ1left, GREATEQ1right)) :: rest671)) => let
 val  result = MlyValue.OP2 (fn _ => (AST.GREATEQ))
 in ( LrTable.NT 8, ( result, GREATEQ1left, GREATEQ1right), rest671)

end
|  ( 36, ( ( _, ( _, LESS1left, LESS1right)) :: rest671)) => let val  
result = MlyValue.OP2 (fn _ => (AST.LESS))
 in ( LrTable.NT 8, ( result, LESS1left, LESS1right), rest671)
end
|  ( 37, ( ( _, ( _, LESSEQ1left, LESSEQ1right)) :: rest671)) => let
 val  result = MlyValue.OP2 (fn _ => (AST.LESSEQ))
 in ( LrTable.NT 8, ( result, LESSEQ1left, LESSEQ1right), rest671)
end
|  ( 38, ( ( _, ( _, EQUAL1left, EQUAL1right)) :: rest671)) => let
 val  result = MlyValue.OP2 (fn _ => (AST.EQUAL))
 in ( LrTable.NT 8, ( result, EQUAL1left, EQUAL1right), rest671)
end
|  ( 39, ( ( _, ( _, AND1left, AND1right)) :: rest671)) => let val  
result = MlyValue.OP2 (fn _ => (AST.AND))
 in ( LrTable.NT 8, ( result, AND1left, AND1right), rest671)
end
|  ( 40, ( ( _, ( _, OR1left, OR1right)) :: rest671)) => let val  
result = MlyValue.OP2 (fn _ => (AST.OR))
 in ( LrTable.NT 8, ( result, OR1left, OR1right), rest671)
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
fun ADD (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun SUB (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
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
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun OR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun TT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun FF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun ASSIGN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun PROGT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun VAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun INT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun BOOL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun LISTYPE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun BRACEL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun BRACER (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
fun READ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.VOID,p1,p2))
fun WRITE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.VOID,p1,p2))
fun WHILE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.VOID,p1,p2))
fun DO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.VOID,p1,p2))
fun ENDWH (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.VOID,p1,p2))
fun TERM (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(
ParserData.MlyValue.VOID,p1,p2))
fun NUM (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(
ParserData.MlyValue.NUM (fn () => i),p1,p2))
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(
ParserData.MlyValue.ID (fn () => i),p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 34,(
ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 35,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 36,(
ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 37,(
ParserData.MlyValue.VOID,p1,p2))
fun ENDIF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 38,(
ParserData.MlyValue.VOID,p1,p2))
fun NEGATE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 39,(
ParserData.MlyValue.VOID,p1,p2))
fun NOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 40,(
ParserData.MlyValue.VOID,p1,p2))
fun COLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 41,(
ParserData.MlyValue.VOID,p1,p2))
end
end
