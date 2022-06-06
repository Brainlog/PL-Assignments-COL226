use "sign.sml";
use "load-code.sml";
open AST
open FunStack 
   
(* The datatype valuestackelem is used for the value stack, the value 
stack will contain several other datatypes and to merge them all 
I will use this datatypev valuestackelem and hence the Value Stack will have elements of type valuestackelem.
The datatype pfix is for the merging several datatypes to single pfix which will contain all the datatypes. *)


    datatype valuestackelem = K of int | pList of AST.command list | IB of vartype | L of bool | E of fac
    datatype pfix = Fac of fac | Li of command list | read  | write | ite  | wh  | set | Idf of string
    (*
The postfix is a recursive function which will take one one element and push it on stack in the way that they are postfix
My parseFile output will be of type command list.
The postfix function will be recursive function. I have used case on command by using pattern matching with several commands. *)
 fun boolconvertor(a)=if a then 1 else 0
    fun postfix([]):pfix Stack=create()
    |postfix(x::xs)=
        case x of
            Command2(s) => push(Idf s,push(read,postfix(xs)))
            | Command3(e) => push(Fac e,push(write,postfix(xs)))
            | Command1(s,e) => push(Idf s,push(Fac e,push(set,postfix(xs)))) 
            | Command4(e,ca,cb) => 
             push(Fac e,push(Li ca,push(Li cb,push(ite,postfix(xs))))) 
            | Command5(e,c) =>
            push(Fac e,push(Li c,push(wh,postfix(xs)))) 

(* Evaluator *)
(* The evaluator has the helper functions and the main functions stackdecider, the stackdecider will contain all the cases and will check which whether the element match any of the cases.
It will take out the element from control stack and accor to the cases will execute recursivley.
The helper functions binary evalve, unary evalve and other evaluationg functions have the cases for different cases of expression.  *)
    
    fun expressionevaluate(e:fac,M)= case e of Fac00 i => Intv  i | Fac02 i => Boolv i | Fac04(e1,b,e2) => op2eval(e1,b,e2,M) | Fac3 (u,e1) => unanEval2(u,e1,M)  | Fac2 (u,e1) => unanEval1(u,e1,M)  | Fac1 (e) => expressionevaluate(e,M)  | Fac01 i =>   let val (A,B)=HashTable.lookup symbolTable i val yt=Array.sub(M,B)
                in
                    case A of
                        INT => Intv (yt)
                        | BOOL => if yt=1 then Boolv (true) else if yt=0 then Boolv (false) else raise Fail("Using wrong operator")
                end
       and unanEval1(u,e,M)=
        case (u,expressionevaluate(e,M)) of
        (NOT,Boolv i) => Boolv (not i)
       
        | _ => raise Fail "Invalid type"

         and op2eval(e1,b,e2,M)=
        case (expressionevaluate(e1,M),b,expressionevaluate(e2,M)) of  (Intv i1,PLUS,Intv i2) => Intv (i1+i2)   | (Intv i1,MINUS,Intv i2) => Intv (i1-i2)    | (Intv i1,MUL,Intv i2) => Intv (i1*i2)    | (Intv i1,DIV,Intv i2) => Intv (i1 div i2)   | (Intv i1,REM,Intv i2) => Intv (i1 mod i2)   | (Boolv i1,AND,Boolv i2) => Boolv (i1 andalso i2)    | (Boolv i1,OR,Boolv i2) => Boolv (i1 orelse i2)
            | (Intv i1,LESS,Intv i2) => Boolv (i1 < i2)
            
              | (Boolv i1,LESS,Boolv i2) 
              => Boolv (boolconvertor(i1) < boolconvertor(i2))
            | (Boolv i1,LESSEQ,Boolv i2)
             => Boolv (boolconvertor(i1) <= boolconvertor(i2))
            | (Boolv i1,EQUAL,Boolv i2)
             => Boolv (boolconvertor(i1) = boolconvertor(i2))
            | (Boolv i1,GREAT,Boolv i2)
             => Boolv (boolconvertor(i1) > boolconvertor(i2))
            | (Boolv i1,GREATEQ,Boolv i2)
             => Boolv (boolconvertor(i1) >= boolconvertor(i2))
           
            | (Intv i1,LESSEQ,Intv i2) => Boolv (i1 <= i2)
            | (Intv i1,EQUAL,Intv i2) => Boolv (i1 = i2)
            | (Intv i1,GREAT,Intv i2) => Boolv (i1 > i2)
            | (Intv i1,GREATEQ,Intv i2) => Boolv (i1 >= i2)
           
          
            | _ => raise Fail "Invalid Type" 

    and unanEval2(u,e,M) = 
    case (u,expressionevaluate(e,M)) of
      
        (NEGATE,Intv i) => Intv (0-i)
        | _ => raise Fail "Invalid type"
            
  
  (* val memo = Array.array(numstates+numstackdecider,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle General.Subscript => ()
          end

val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(List.map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numstackdecider=numstackdecider,
numStates=numstates,initialState=STATE 0}
end
end

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
*)

(*
The function stackdecider will check for the command from  postfix stack
*)

    fun stackdecider(V:valuestackelem FunStack.Stack,M,C)=
        if empty(C) then (V,M,C)
        else
            let val c=top(C)
            in
                case c of
              (*
              The first case is when we found a command of expression type, in this case we will push the evaluated expression in value stack
              and from there for next case will assume there is something in value stack and for next command will execute the value stack and put the result in memory stack. As in hash table we have given a memory index to
              every string. To access any element we have goto that location in memory stack
              *)
                 Fac c =>  let val res=expressionevaluate(c,M); in  case res of Intv i => stackdecider(push(E c,push(K i,V)),M,pop(C))  |Boolv i => stackdecider(push(E c,push(L i,V)),M,pop(C))  end
                    (*
                    This is the case when we find the list of commands
                    *)
                | Li c => stackdecider(push(pList c,V),M,pop(C))
                | read => let val inp=valOf(Int.fromString(valOf(TextIO.inputLine TextIO.stdIn)))
                 val i=top(pop(V))
                   in case (top(V),i) of 
                    (IB BOOL,K i) => if inp=1 orelse inp=0 then
                     let val it1=Array.update(M,i,inp)
                     in stackdecider(pop(pop(V)),M,pop(C)) 
                     end
                      else raise Fail("Invalid Stack") 
                      | (IB INT,K i) => (Array.update(M,i,inp); 
                      stackdecider(pop(pop(V)),M,pop(C)))  
                       | _ => raise Fail "Semantic Error : check the read statement"    end
                | Idf y => let val (I,J)=HashTable.lookup symbolTable y;  in stackdecider(push(IB I,push(K J,V)),M,pop(C)) end     

                | write
                 =>  let val y=top(pop(V)) in
                  case (y) of (K imyval) => 
                  let val it=print(Int.toString(imyval)) in
                   stackdecider(pop(pop(V)),M,pop(C)) end
                    | (L imyval) => let val it=print(Int.toString(boolconvertor(imyval))) 
                    in stackdecider(pop(pop(V)),M,pop(C)) 
                    end 
                    | _ => raise Fail "Semantic Error : check the write statement"    end                    
                | set =>  
                 let val j=top(pop(pop(pop(V)))); 
                 val itu = print(""); 
                 in 
                 case (top(pop(V)),top(pop(pop(V))),j) of (K i,IB INT,K j) 
                 => let val it=Array.update(M,j,i);  in stackdecider(pop(pop(pop(pop(V)))),M,pop(C)) end | (L i,IB BOOL,K j) => let val it=Array.update(M,j,boolconvertor(i)) in stackdecider(pop(pop(pop(pop(V)))),M,pop(C)) end  | _ => raise Fail "Semantic Error : check the set statement" end                   
                | ite =>   let val k=top(V) in case (k,top(pop(V)),top(pop(pop(pop(V))))) of (pList c1,pList c2,L i) => if i then stackdecider(pop(pop(pop(pop(V)))),M,FunStack.pushList(postfix(c2),pop(C))) else stackdecider(pop(pop(pop(pop(V)))),M,pushList(postfix(c1),pop(C)))  | _ => raise Fail "Semantic Erro : check the if then else statement"  end
                | wh =>    let val j=top(pop(V)); in    case (top(V),top(pop(pop(V))),j) of    (pList c1,L i,E j) => if i then stackdecider(pop(pop(pop(V))),M,FunStack.pushList(postfix(c1),push(Fac j,push(Li c1,C))))     else stackdecider(pop(pop(pop(V))),M,pop(C))    | _ => raise (Fail "\n Check your while statement")       end
            end


      fun string_recurse( e : fac) = 
    case e of 
    Fac2(NOT,e) => "!"^string_recurse(e)
    | Fac00(i) => Int.toString(i)
    | Fac01(i) => i
    | Fac02(i) => if i then "true" else "false"
    | Fac3(NEGATE,e) => "~"^string_recurse(e)    
    | Fac1(e) => "("^string_recurse(e)^")"
    | Fac04(exp1,PLUS,exp2) => string_recurse(exp1)^"+"^string_recurse(exp2)
    | Fac04(exp1,MINUS,exp2) => string_recurse(exp1)^"-"^string_recurse(exp2)
    | Fac04(exp1,MUL,exp2) => string_recurse(exp1)^"*"^string_recurse(exp2)
    | Fac04(exp1,DIV,exp2) => string_recurse(exp1)^"/"^string_recurse(exp2)
    | Fac04(exp1,REM,exp2) => string_recurse(exp1)^"%"^string_recurse(exp2)
    | Fac04(exp1,AND,exp2) => string_recurse(exp1)^"$$"^string_recurse(exp2)
    | Fac04(exp1,OR,exp2) => string_recurse(exp1)^"||"^string_recurse(exp2)
    | Fac04(exp1,LESS,exp2) => string_recurse(exp1)^"<"^string_recurse(exp2)
    | Fac04(exp1,GREAT,exp2) => string_recurse(exp1)^">"^string_recurse(exp2)
    | Fac04(exp1,EQUAL,exp2) => string_recurse(exp1)^"+"^string_recurse(exp2)
    | Fac04(exp1,LESSEQ,exp2) => string_recurse(exp1)^"<="^string_recurse(exp2)
    | Fac04(exp1,GREATEQ,exp2) => string_recurse(exp1)^">="^string_recurse(exp2)
    (* | Fac04(exp1,NOT,exp2) => string_recurse(exp1)^"+"^string_recurse(exp2) *)
    | _ => ""    
   
   
     and string_recurse2([]) = "$$" |
     string_recurse2(z::l : command list) = "$$"^command_convert(z)^string_recurse2(l)
     
    and command_convert(f : command) : string = 
    case f of 
      Command1(a,e) => "set" ^ a ^ ":=" ^ string_recurse(e)
    | Command2(s) => "read" ^ s 
    | Command3(e) => "write " ^ string_recurse(e)
    | Command4(e1,c1,c2) => "if " ^ string_recurse(e1) ^ " then " ^ string_recurse2(c1) ^ " else " ^ string_recurse2(c2) ^ " endif "
    | Command5(e1,c1) => "while" ^ string_recurse(e1) ^ " do " ^ string_recurse2(c1) ^ " endwh "

    


    and  String_convert(A : valuestackelem) = 
    case A of
     K i => Int.toString(i)
     | IB INT => "int"
     | IB BOOL => "bool"
     | E e => string_recurse(e)
     | pList c => string_recurse2(c)
     | _ => ""
     
  

    and  String_convert_2(A : pfix) = 
    case A of
        read => "read"
       | set => "set"
       | write => "write"
       | wh => "wh"
       | ite => "ite"
       | Idf a => a
       | Li l => string_recurse2(l)
       | Fac e => string_recurse(e);


   
    fun execute(filename : string) = 
      let val V: valuestackelem FunStack.Stack  = create();
          val M = Array.array(100,0);
          val (A,_) = parseFile(filename);
          val C = postfix(A);
          
          
          in
          stackdecider(V,M,C)
          end
    (* signature VMC =
   sig
     val toString : valuestackelem FunStack.Stack * int array * pfix FunStack.Stack  -> unit
     val run : string -> string * int array * string
   end         *)

   
 
  
   fun toString((V,M,C)) =
   let 
    
   val (A,B,C) =  (FunStack.toString String_convert V, M, FunStack.toString String_convert_2 C);  
    val it1 = print("THE STATUS OF VALUE STACK IS --> \n\n\n"); 
    val it2 = print(A);
   val it10 = print("\n\n");
   val it60 = print("----");
   val it5 = print("THE STATUS OF CONTROL STACK IS --> \n\n\n");
   val it12 = print(C);
   val it50 = print("----")
   val it13 = print("\n\n");
   val it14 = print("THE STATUS OF MEMORY STACK IS --> \n\n")
  

   in
   ()
    end;

   fun runcode(filename) = (execute(filename),toString(execute(filename))); 

    
     
   
 