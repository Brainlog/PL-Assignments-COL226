signature STACK =
sig
type 'a Stack
exception EmptyStack
exception Error of string
val create : unit -> 'a Stack
val push : 'a * 'a Stack -> 'a Stack
val pop : 'a Stack -> 'a Stack
val top : 'a Stack -> 'a
val empty: 'a Stack -> bool
val poptop : 'a Stack -> ('a * 'a Stack) option
val nth : 'a Stack * int -> 'a
val drop : 'a Stack * int -> 'a Stack
val depth : 'a Stack -> int
val app : ('a -> unit) -> 'a Stack -> unit
val map : ('a -> 'b) -> 'a Stack -> 'b Stack
val mapPartial : ('a -> 'b option) -> 'a Stack -> 'b Stack
val find : ('a -> bool) -> 'a Stack -> 'a option
val filter : ('a -> bool) -> 'a Stack -> 'a Stack
val foldr : ('a * 'b -> 'b) -> 'b -> 'a Stack -> 'b
val foldl : ('a * 'b -> 'b) -> 'b -> 'a Stack -> 'b
val exists : ('a -> bool) -> 'a Stack -> bool
val all : ('a -> bool) -> 'a Stack -> bool
val list2stack : 'a list -> 'a Stack (* Convert a list into a stack *)
val stack2list: 'a Stack -> 'a list (* Convert a stack into a list *)
val pushList : 'a Stack * 'a Stack -> 'a Stack
val toString: ('a -> string) -> 'a Stack -> string
end

(* 
structure FunStack :> STACK = 
struct
  type 'a Stack = 'a list
  fun create() = [];
  fun push(a,S) = a::S;
  fun pop([]) = [] | pop(a::S) = S;
  fun top([]) = [] | top(a::S) = a;
  fun empty(S) = if S = [] then true else false;
  fun poptop(a::S) = if a::S = [] then NONE else SOME (a,S);
  fun nth(S,n) = (List.nth(S,n));
  fun drop(a::S,n) = List.drop(a::S , n);
  fun depth([]) = 0 |
  depth(a::S) 
  if a::S = [] then c else depthhl(S,c+1)
  in 
   if S = [] then 1 else depthhl(a::S,0)
   end;
  fun app(f,S) = (List.app f S);
  
  fun map(f,S) = (List.map f S);
  fun mapPartial(f,S) = (List.mapPartial f S);
  fun find(f,S) = (List.find f S);
 ringhl(f,S,f(a)@L) | toStringhl(a2s,[],L) = L;
  fun a2s(a) = 
  in 
  toStringhl(a2s,S,"")
  end;


end   *)

structure FunStack :> STACK =
struct
    type 'a Stack = 'a list
    exception EmptyStack
    exception Error of string
    fun create()=[]
    fun push(a,S)=a::S
    fun pop([])=raise EmptyStack
    | pop(a::S)=S
    fun top([])=raise EmptyStack
    | top(a::S)=a
    fun empty([])=true
    | empty(a::S)=false
   
    fun app f S=List.app f S
    fun map f S=List.map f S
    fun toString a2s S=
        let val L=map a2s S  fun conc(s1,s2)=s1^" "^s2
        in foldr conc "" L
        end;
    fun mapPartial f L=List.mapPartial f L 
    fun exists f L=List.exists f L
    fun all f S=List.all f S
     fun poptop([])=NONE
    | poptop(a::L)=SOME (a,L)
    fun nth(S,i)=List.nth(S,i)
    fun drop(S,i)=List.drop(S,i)
    fun depth([])=0
    | depth(a::S)=1+depth(S)
     fun find g L=List.find g L
    fun filter f L=List.filter f L
    fun foldr f initialvalue S=List.foldr f initialvalue S
    fun foldl f initialvalue S=List.foldl f initialvalue S
    fun list2stack(S)= S
    fun stack2list(S)=S
    fun pushList(L,S)=L@S
   

end