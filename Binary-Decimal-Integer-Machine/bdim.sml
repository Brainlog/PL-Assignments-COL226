exception Fail;
exception DividebyZero;
exception OPCodedonotexist;
exception IndexNegative;
exception IncorrectInputQuadruple;
exception BooleanExcepted;
fun error(str : string) =
let
val it = print(str)
in
OS.Process.exit(OS.Process.success)
end;


fun register(k, lis ,c , d ) =
if k < 0 then raise IndexNegative
else if  not(lis = []) andalso c = k  then d::tl(lis)
else if c = k  then d::(lis)
else if lis = [] then 0::register(k ,  lis, c + 1,d) 
else hd(lis) :: register(k , tl(lis), c + 1,d);
fun access(k, head :: tail , c ) =
if k < 0 then raise IndexNegative 
else if c = k then SOME head
else if tail = [] then NONE
else access(k,tail,c + 1);
fun op0(t : int list list) = [];
fun op1(k : int, l : int list, code : int list list, status : int) = 
let
val y = print("input:")
val str = valOf (TextIO.inputLine TextIO.stdIn)
val  x = String.size(str)
val value = valOf(Int.fromString(String.substring(str,0,x-1)))
in
 (valOf(access(status, code, 0)) , register(k,l,0,value), status + 1)
end; 
fun op2(i : int, k : int, l : int list, code : int list list, status : int) = 
let 
val value = access(i,l,0)
val newlist = register(k,l,0,valOf(value))
in 
 (valOf(access(status, code, 0)) , newlist, status + 1)
end;
fun op3(i : int, k : int, l : int list, code : int list list, status : int) = 
let
val value = access(i,l,0)
val newValue = if valOf(value) = 1 then 0 else 1
val newList = register(k,l,0,newValue)
in
(valOf(access(status, code, 0)) , newList , status + 1)
end;
fun op4(i : int, j : int, k : int, l : int list, code : int list list, status : int) = 
let 
val value = access(i,l,0)
val value2 = access(j,l,0)
val newValue = if valOf(value) = 0 andalso valOf(value2) = 0 then 0 else 1
val newList = register(k,l,0,newValue)
in 
(valOf(access(status, code, 0)) , newList, status + 1)
end;
fun op5(i : int, j : int, k : int, l : int list, code : int list list, status : int) = 
let 
val value = access(i,l,0)
val value2 = access(j,l,0)
val newValue = if valOf(value) = 1 andalso valOf(value2) = 1 then 1 else 0
val newList = register(k,l,0,newValue)
in 
(valOf(access(status, code, 0)) , newList, status + 1)
end;
fun op6(i : int, j : int, k : int, l : int list, code : int list list, status : int) = 
let 
val value = access(i,l,0)
val value2 = access(j,l,0)
val newValue = valOf(value) + valOf(value2)
val newList = register(k,l,0,newValue)
in 
(valOf(access(status, code, 0)) , newList, status + 1)
end;
fun op7(i : int, j : int, k : int, l : int list, code : int list list, status : int) = 
let
val value = access(i,l,0)
val value2 = access(j,l,0)
val newValue = valOf(value) - valOf(value2)
val newList = register(k,l,0,newValue)
in 
(valOf(access(status, code, 0)) , newList, status + 1)
end;
fun op8(i : int, j : int, k : int, l : int list, code : int list list, status : int) = 
let 
val value = access(i,l,0)
val value2 = access(j,l,0)
val newValue = valOf(value)*valOf(value2)
val newList = register(k,l,0,newValue)
in 
(valOf(access(status, code, 0)) , newList, status + 1)
end;
fun op9(i : int, j : int, k : int, l : int list, code : int list list, status : int) = 
let 
val value = access(i,l,0)
val value2 = access(j,l,0)
val newValue = if valOf(value2) = 0 then raise DividebyZero else valOf(value) div valOf(value2) handle DividebyZero => error("Can't divide by Zero")
val newList = register(k,l,0,newValue)
in 
(valOf(access(status, code, 0)) , newList, status + 1)
end;
fun op10(i : int, j : int, k : int, l : int list, code : int list list, status : int) = 
let 
val value = access(i,l,0)
val value2 = access(j,l,0)
val newValue = valOf(value) mod valOf(value2)
val newList = register(k,l,0,newValue)
in 
(valOf(access(status, code, 0)) , newList, status + 1)
end;
fun op11(i : int, j : int, k : int, l : int list, code : int list list, status : int) = 
let 
val value = access(i,l,0)
val value2 = access(j,l,0)
val newValue = if  valOf(value) = valOf(value2) then 1 else 0
val newList = register(k,l,0,newValue)
in 
(valOf(access(status, code, 0)) , newList, status + 1)
end;
fun op12(i : int, j : int, k : int, l : int list, code : int list list, status : int) = 
let
val value = access(i,l,0)
val value2 = access(j,l,0)
val newValue = if  valOf(value) > valOf(value2) then 1 else 0
val newList = register(k,l,0,newValue)
in 
(valOf(access(status, code, 0)) , newList, status + 1)
end;
fun op13(i : int, c :int, l : int list, code : int list list, status : int) = 
let 
val value = access(i,l,0)
val check = if valOf(value) = 0 then 0 else 1
in 
if check = 0 andalso c = status + 1 then
    (valOf(access(status,code,0)), l, status + 2)
else if check = 0 andalso not(c = status + 1) then 
    (valOf(access(status,code,0)), l, status + 1)
else if check = 1 andalso c = status + 1 then 
    (valOf(access(status,code,0)), l, status + 1)  
else
    (valOf(access(c,code,0)), l,  c)
end;


fun op14(c :int, l : int list, code : int list list, status : int) = 
let 
val value = access(status,code,0)
in
(valOf(value) , l, c)
end;


fun op15(i: int, l: int list, code : int list list, status : int) = 
let 
val value = access(i,l,0)
val it = print(Int.toString(valOf(value)))
in 
(valOf(access(status, code, 0)) , l, status + 1 )
end;


fun op16(v : int, k: int, l : int list, code : int list list, status : int) = 
let 
val newList = register(k,l,0,v) 

in 
(valOf(access(status, code, 0)) , newList, status + 1 )
end;


fun lazy(filein:string, c : int) = 
let 
val tem = TextIO.openIn(filein)
val str = if c = 0 then SOME tem else NONE
in
str
end;
fun mlazy(filein : string) =
let 
val str = valOf(lazy(filein,0))
in 
str
end; 
fun filetolist(filein : string, code , c: int) = 
let 
val str =  mlazy(filein) 
fun readThis(str, code, c : int) = 
let
val sr = (TextIO.inputLine(str))
val f = if not(sr = NONE) then String.size(valOf(sr)) else 0
val g = if not (sr = NONE) then String.substring(valOf(sr), 0, f-1) else ""
in
if sr = NONE then code else readThis(str,code @ [g], c+1)
end;
in
readThis(str, code, c)
end;
fun strparser(sr : string, k : int, entry : string, tuple) = 
let
val size = String.size(sr)
val kaneki = String.str(String.sub(sr, k))
in
if String.sub(sr,k) = #")" andalso not(String.sub(sr,k-1) = #"_")   then tuple @ [valOf(Int.fromString(entry^kaneki))]
else if String.sub(sr,k) = #")" then tuple
else if String.sub(sr,k) = #"_" then strparser(sr, k+1, "", tuple@[100])
else if String.sub(sr,k) = #"," then strparser(sr, k+1, "", tuple)
else if String.sub(sr,k+1) = #"," then strparser(sr,k+1, "", tuple@[valOf(Int.fromString(entry^kaneki))])
else if String.sub(sr,k) = #"(" then strparser(sr, k+1, "", tuple)
else if String.sub(sr,k) = #"-" then strparser(sr, k+1, entry^"~" , tuple)
else strparser(sr,k+1,entry^kaneki, tuple)
end;
fun ilistTuple(l : string list, a : int list list) =
let
in
if l = [] then a else ilistTuple(tl(l),a@[strparser(hd(l),0,"",[])]) 
end;
fun coderun(code: int list list,l : int list , status : int) =
let 
val extra = print("")
val a = hd(valOf(access(status,code,0)))
val b = hd(tl(valOf(access(status,code,0))))
val c = hd(tl(tl(valOf(access(status,code,0)))))
val d = hd(tl(tl(tl(valOf(access(status,code,0))))))
val x = 
if a = 1 then (op1(d,l,code,status))
else if a = 2 then (op2(b,d,l,code,status))
else if a = 3 then (op3(b,d,l,code,status))
else if a = 4 then (op4(b,c,d,l,code,status))
else if a = 5 then (op5(b,c,d,l,code,status))
else if a = 6 then (op6(b,c,d,l,code,status))
else if a = 7 then (op7(b,c,d,l,code,status)) 
else if a = 8 then (op8(b,c,d,l,code,status))
else if a = 9 then (op9(b,c,d,l,code,status))
else if a = 10 then (op10(b,c,d,l,code,status))
else if a = 11 then (op11(b,c,d,l,code,status))
else if a = 12 then (op12(b,c,d,l,code,status))
else if a = 13 then (op13(b,d,l,code,status))
else if a = 14 then (op14(d,l,code,status))
else if a = 16 then op16(b,d,l,code,status)  
else if a = 15 then op15(b,l,code,status)
else if a =0 then OS.Process.exit(OS.Process.success) 
else ([], l, status);
val (p,q,r) = x
in 
coderun(code, q, r)
end;
fun interpret(filein : string) = 
let
val a = coderun(ilistTuple(filetolist(filein,[],0),[]),[],0)
in 
a
end;

fun main() = 
let
val y = print("Enter the function")
val inp = valOf (TextIO.inputLine TextIO.stdIn);
val x = String.size(inp)
val filename = String.substring((inp), 11, x-14)
in 
interpret(filename)
end;

main();



