/*HELPER PREDICATES*/
/* max is predicate for just comparing two numbers */
max(X,Y,N) :- Y>=X, N is Y.
max(X,Y,N) :- X>Y, N is X.


/* This is just a helper predicate which compares two List and is true if Both Lists are equal*/
listeq([ ], [ ]).
listeq([H1|R1], [H2|R2]):-
    H1 == H2,
    listeq(R1, R2).


/* This is just a helper predicate which is True if C is absolute differnce of X and Y */
abs(X,Y,C):- X>=Y,C is X-Y.
abs(X,Y,C):- Y>X,C is Y-X.



find(X,[],C) :- C is 0.
find(X,[H|T],C) :- (H = X, C is 1);(find(X,[T],D), C is D).


chkIndi(X,[],J,Counter) :- J = -1.
chkIndi(X,[H|T],J,Counter) :-(X = H, J is Counter);(chkIndi(X,T,Got,Counter+1),J is Got).

chkmember(X,A,C,[]):- A = C.
chkmember(X,A,C,[H|T]) :- (X = H, Cto is C+1, chkmember(X,A,Cto,T));(chkmember(X,A,C,T)).
preET_h(S,[],A) :- A=S.
preET_h(S,[H|T],A) :- (member(H,S), preET_h(S,T,A));( append(S,[H],S1),(preET_h(S1,T,A))).
inET_h(S,[],A) :- A =S.
inET_h(S,[H|T],A):- (chkmember(H,C,0,[H|T]), C = 3, inET_h(S,T,A));(chkmember(H,C,0,[H|T]), C = 2, append(S,[H],S1), inET_h(S1,T,A));(chkmember(H,C,0,[H|T]), C=1, inET_h(S,T,A)).
postET_h(S,[],A) :- A = S.
postET_h(S,[H|T],A) :- (chkmember(H,C,0,[H|T]), C = 3, postET_h(S,T,A));(chkmember(H,C,0,[H|T]), C = 2, postET_h(S,T,A));(chkmember(H,C,0,[H|T]), C=1,append(S,[H],S1), postET_h(S1,T,A)).


right_min(ibt(node(X,empty,R)),A,S) :-  A is X, S = ibt(R).
right_min(ibt(node(X,node(L,empty,R_l),R)),A, S) :- A is L, S = ibt(node(X,R_l,R)).
right_min(ibt(node(X,L,R)),A,S) :- right_min(ibt(L),B,ibt(C)), A=B, S = ibt(node(X,C,R)).



slice(I,J,[H|T],A,Counter,List) :- (Counter =0, length([H|T],N),Nmi is N-1 ,I > Nmi, List = []).
slice(I,J,[],A,Counter,List):- (List = A).
slice(I,J,[H|T],A,Counter,List) :-
    ( I = J, I = 0, List = []  );
    (I < J, Counter < I,
     F is Counter+1,
     slice(I,J,T,A,F,List)
    );
    (I < J,
     Counter = I,
     append(A,[H],G),
     F is Counter+1,
     slice(I,J,T, G, F, List)
    );
    (I <J,
     Counter > I,
     Counter < J,
     append(A,[H],Mylist),
     UrList = Mylist ,
     F is Counter+1,
     slice(I,J,T,UrList,F,List)
    );
    (I < J,
     Counter > I,
     Counter = J,
     List = A
    ).

labelInt(node(X,Left,Right),Value,NodeLeft,NodeRight):- Value = X, NodeLeft = Left, NodeRight = Right.

trPreorder_h(ibt(node(X,empty,empty)),L,S,C,Lmain) :- Lmain = [X].
trPreorder_h(ibt(empty),L,S,C,Lmain) :- Lmain = [].
trPreorder_h(ibt(node(X,Left,R)),L,S,C,Lmain) :- (C=0, append(L,[X],L1), append(S,[R,Left],S1), trPreorder_h(ibt(node(X,Left,R)),L1,S1,1,Lmain));(S=[],Lmain = L);(length(S,N), Nmin is N-1, slice(0,Nmin,S,[],0,Spopped), slice(Nmin,N,S,[],0,[Spop]), labelInt(Spop,Val,NLeft,NRight), NLeft = empty, NRight = empty, append(L,[Val], L1), trPreorder_h(ibt(node(X,L,R)),L1,Spopped,1,Lmain));( length(S,N), Nmin is N-1, slice(0,Nmin,S,[],0,Spopped), slice(Nmin,N,S,[],0,[Spop]), labelInt(Spop,Val,NLeft,NRight), NLeft = empty, append(L,[Val],L1), append(Spopped,[NRight], Spoppednew), trPreorder_h(ibt(node(X,L,R)),L1,Spoppednew,1,Lmain));
( length(S,N), Nmin is N-1, slice(0,Nmin,S,[],0,Spopped), slice(Nmin,N,S,[],0,[Spop]), labelInt(Spop,Val,NLeft,NRight), NRight = empty, append(L,[Val],L1), append(Spopped,[NLeft], Spoppednew), trPreorder_h(ibt(node(X,L,R)),L1,Spoppednew,1,Lmain));
( length(S,N), Nmin is N-1, slice(0,Nmin,S,[],0,Spopped), slice(Nmin,N,S,[],0,[Spop]), labelInt(Spop,Val,NLeft,NRight), append(L,[Val],L1), append(Spopped,[NRight,NLeft],Spoppednew), trPreorder_h(ibt(node(X,L,R)),L1,Spoppednew,1,Lmain)).

trPostorder_h(ibt(node(X,empty,empty)),L,S,C,Lmain) :- Lmain = [X].
trPostorder_h(ibt(empty),L,S,C,Lmain) :- Lmain = [].
trPostorder_h(ibt(node(X,Left,R)),L,S,C,Lmain) :-  (C=0, append(L,[X],L1), append(S,[Left,R],S1), trPostorder_h(ibt(node(X,Left,R)),L1,S1,1,Lmain));(S=[],reverse(L,L1),Lmain = L1);(length(S,N), Nmin is N-1, slice(0,Nmin,S,[],0,Spopped), slice(Nmin,N,S,[],0,[Spop]), labelInt(Spop,Val,NLeft,NRight), NLeft = empty, NRight = empty, append(L,[Val], L1), trPostorder_h(ibt(node(X,L,R)),L1,Spopped,1,Lmain));( length(S,N), Nmin is N-1, slice(0,Nmin,S,[],0,Spopped), slice(Nmin,N,S,[],0,[Spop]), labelInt(Spop,Val,NLeft,NRight), NLeft = empty, append(L,[Val],L1), append(Spopped,[NRight], Spoppednew), trPostorder_h(ibt(node(X,L,R)),L1,Spoppednew,1,Lmain));
( length(S,N), Nmin is N-1, slice(0,Nmin,S,[],0,Spopped), slice(Nmin,N,S,[],0,[Spop]), labelInt(Spop,Val,NLeft,NRight), NRight = empty, append(L,[Val],L1), append(Spopped,[NLeft], Spoppednew), trPostorder_h(ibt(node(X,L,R)),L1,Spoppednew,1,Lmain));

( length(S,N), Nmin is N-1, slice(0,Nmin,S,[],0,Spopped), slice(Nmin,N,S,[],0,[Spop]), labelInt(Spop,Val,NLeft,NRight), append(L,[Val],L1), append(Spopped,[NLeft,NRight],Spoppednew), trPostorder_h(ibt(node(X,L,R)),L1,Spoppednew,1,Lmain)).

trInorder_h(ibt(empty),L,S,C,Lmain) :- Lmain = [].
trInorder_h(ibt(node(X,empty,empty)),L,S,C,Lmain) :- Lmain = [X].
trInorder_h(ibt(node(X,Left,R)),L,S,Curr,Lmain) :- (L =[], S = [], Curr = node(X,Left,R), append(S,[node(X,Left,R)],S1),Curr1 =Left, trInorder_h(ibt(node(X,Left,R)),L,S1,Curr1,Lmain));(S = [],Curr = empty ,L =[H|_],Lmain = L);(Curr = empty,length(S,N),Nmimi is N-1, slice(Nmimi,N,S,[],0,[Spopped]), slice(0,Nmimi,S,[],0,Spop),labelInt(Spopped,Val,Nleft,Nright), append(L,[Val], L1), Curr3 = Nright,trInorder_h(ibt(node(X,L,R)),L1,Spop,Curr3,Lmain));(labelInt(Curr,Val,NLeft,NRight),Curr2 = NLeft,append(S,[Curr],S1), trInorder_h(ibt(node(X,Left,R)),L,S1,Curr2,Lmain)).


makeBST_h(L, A,C,L0) :- (L0=[],A =ibt(empty))
;(length(L0,N),N=1,L0=[X],A = ibt(node(X,empty,empty))); (C =0, sort(L0,SoL),  makeBST_h(L,A,1,SoL));( length(L0,N),J is N//2, K is J+1, slice(0,J,L0,[],0,Mylist1),  slice(J,K, L0,[], 0,[H|_]), slice(K, N, L0,[], 0,Mylist3), makeBST_h(L,ibt(Left),1,Mylist1), makeBST_h(L,ibt(Right),1,Mylist3), A = ibt(node(H,Left,Right))).




/* Code : Assignment 2 */

/* Binary Tree Implementation using Recursion */
ibt(empty).
ibt(node(X,L,R)) :- (integer(X), ibt(L),ibt(R)).

/* Using Recursion, for empty case We will have 0 as size, for tree with single node we will have 1 as a size, And for any other tree recursively, we will have (1 + size(Left Tree) + size(Right Tree)) */
size(ibt(empty),0).
size(BT,N) :- (BT = ibt(node(X,L,R))),(integer(X), size(ibt(L),M), size(ibt(R),P), N is  M + P +1).

/* height is recursive rule which is true for empty tree and for one node tree, and for height other than this we will have compare the arguments of height for left and right subtree and add one to it*/
height(ibt(empty),0).
height(BT,N) :-(BT = ibt(node(X,L,R))),(integer(X), height(ibt(L),M), height(ibt(R),P), max(M,P,T), N is T+1).

/* preorder is a recursive rule that is true for empty tree and [] as arguments, for single node tree it will be true if List has only node value in it, for other than this it will true when preorder of Left subtree and right sub Tree would be correct and then the Lists for which they are correct are appended in a way RoootNode,LeftList,RightList */
preorder(ibt(empty),[]).
preorder(ibt(node(X,empty,empty)),N):- append([X],[],U), N = U.
preorder(ibt(node(X,T,R)),L) :- integer(X), preorder(ibt(T),A), preorder(ibt(R),B), append(A,B,Main1), append([X],Main1,Main2), L = Main2.

/* Inorder is a recursive rule that is true for empty tree and [] as arguments, for single node tree it will be true if List has only node value in it, for other than this it will be true if Inorder for Left is true and Inorder for Right is true and also when the Lists for which Left and Right Tree are appended in a way LeftList,Node,RightList and should be equal to List  */
inorder(ibt(empty),[]).
inorder(ibt(node(X,empty,empty)),N):- append([X],[],U), N = U.
inorder(ibt(node(X,T,R)),L) :- integer(X), inorder(ibt(T),A), inorder(ibt(R),B), append(A,[X],Main1), append(Main1,B,Main2), L = Main2.

/* Postorder is a recursive rule that is true for empty tree and [] as  parameters, for single node tree it will be true if list has only node value in it, for other than this it will be true if Postorder and it will be true for Right Tree, And Lists for which is true when appended with Node label in a way LeftList,RightList,Node and should be equal to List */
postorder(ibt(empty),[]).
postorder(ibt(node(X,empty,empty)),N):- append([X],[],U), N = U.
postorder(BT,L) :- (BT = ibt(node(X,T,R))),(integer(X), postorder(ibt(T),A), postorder(ibt(R),B), append(A,B,Main1), append(Main1,[X],Main2), L = Main2).

/* trPreorder uses a helper function which works iteratively and stores the node by node in a Stack and pop them, after every pop Append them to list */
trPreorder(BT,L):- trPreorder_h(BT,[],[],0,L1), L = L1.

/* Use a Stack, Use a List using same concept as Preorder then Reverse the List but append the nodes in Left Right Fashion */
trPostorder(BT,L) :- trPostorder_h(BT,[],[],0,L1), L = L1.

/* Use a Stack and a current pointer,  the current is pushed in Stack till empty then if empty pop the stack and set current as Right of popped ones and also push it in stack */
trInorder(ibt(node(X,Left,R)),L) :- trInorder_h(ibt(node(X,Left,R)),[],[],node(X,Left,R),L1), L = L1.

/* Visit every node in recursion, for leaf nodes visit them 3 times */
eulerTour(ibt(node(X,empty,empty)),L) :- L = [X,X,X].
eulerTour(ibt(node(X,empty,R)),L) :- eulerTour(ibt(R),A), append([X],A,C1),append([X],C1,C2), append(C2,[X],D),L = D.
eulerTour(ibt(node(X,L,empty)),R) :- eulerTour(ibt(L),A), append([X],A,C), append(C,[X],D1),append(D1,[X],D2),R = D2.
eulerTour(ibt(node(X,T,R)),L) :- eulerTour(ibt(T),A), eulerTour(ibt(R),B), append([X],A,C), append(C,[X],D), append(D,B,E), append(E,[X],F), L = F.

/* Using the euler Tour, We can see that for 1st occurence of every element if we add it to list and ignore otherwise will give a preorder traversal */
preET(A,L) :- eulerTour(A,EL), preET_h([],EL,S), L = S.

/* Use 2nd occurrence */
inET(A,L) :- eulerTour(A,EL), inET_h([],EL,S), L =S.

/* Use 3rd occurrence */
postET(A,L) :- eulerTour(A,EL),  postET_h([],EL,S), L =S.



/* toString is a recursive rule that is true for empty Tree and () as a String, for one node it is true when (X) you have this a String where X is a node, for other than this, we will have predicate true if predicate for Left Sub Tree is true and predicate for Right Sub tree is true and List is equal to (N, String for Left Tree, String for Right Tree) */
toString(ibt(empty),"()").
toString(BT,S) :- (BT = ibt(node(X,L,R))),(toString(ibt(L),A),toString(ibt(R),B),atom_concat("(",X,Z),atom_concat(Z,", ",V),atom_concat(V,A,D),atom_concat(D,", ",F),atom_concat(F,B,K), atom_concat(K,")",N),  S = N).



/* This predicate is True for empty Tree, for one node Tree it is also true, for other Tree it will be true if inorder Traversal of Tree is equal to Sorted Inorder Traversal */
isBST(ibt(empty)).
isBST(ibt(node(X,empty,empty))):- integer(X).
isBST(ibt(node(X,T,R))):- inorder(ibt(node(X,T,R)),L), sort(L,B), listeq(L,B).


/* Slice the list in half and pick the node at half for rootnode and left half as Left Subtree and Right Half as Right Sub Tree */
makeBST(L,A) :- makeBST_h(L,A,0,L).




/* This predicate is True for single node and for other cases it is true if for Left Subtree predicate is True and For RIght Tree also, and the diff of height is less than equal to one. */
isBalanced(ibt(empty)).
isBalanced(ibt(node(X,empty,empty))):- integer(X).
isBalanced(ibt(node(X,L,R))) :- integer(X), height(ibt(L),A), height(ibt(R),B), abs(A,B,C), C <2, isBalanced(ibt(L)), isBalanced(ibt(R)).

/* This predicate will always false for empty Tree, for Other cases if N = X, then true, if N < X, lookup predicate for Left Tree should be correct or if N > X then it should be predicate for Right Sub Tree should be True */
lookup(N,ibt(empty)) :- 2=3, integer(N).
lookup(N,ibt(node(X,L,R))) :- isBST(ibt(node(X,L,R))), N = X.
lookup(N,ibt(node(X,L,R))) :- isBST(ibt(node(X,L,R))), N < X, lookup(N, ibt(L)).
lookup(N,ibt(node(X,L,R))) :- isBST(ibt(node(X,L,R))), N > X, lookup(N, ibt(R)).


/* This predicate is True if you have empty tree and a tree with a node as Argument, for Other cases it is true if N = X, if A = Tree which is given, N < X, insert predicate is true for Left Tree and A = Tree with Node, Left Tree as Tree from predicate and RightTree as R if N > X, VIce Versa */
insert(N,ibt(empty),A) :- A = ibt(node(N,empty,empty)).
insert(N,ibt(node(X,L,R)),A) :- N = X, A = ibt(node(X,L,R)).
insert(N,ibt(node(X,L,R)),A) :- N < X, insert(N, ibt(L),ibt(B)), A = ibt(node(X,B,R)).
insert(N,ibt(node(X,L,R)),A) :- N > X, insert(N, ibt(R),ibt(B)), A = ibt(node(X,L,B)).

/* Using a helper function right min, We will recurse till immediate predecessor and change the tree such that it comes at the position of the node to deleted , if N <X move left or N > X move right*/
delete(N,ibt(node(X,L,R)),A):- (L = empty, R = empty, N = X, A = ibt(empty)); (L = empty, N = X, A = ibt(R)); (R = empty, N = X, A = ibt(L)); (N = X, right_min(ibt(R),T,ibt(U)), A = ibt(node(T,L,U)));(N < X, delete(N,ibt(L),ibt(G_u)), A = ibt(node(X,G_u,R)));(N > X, delete(N,ibt(R),ibt(G_r)), A = ibt(node(X,L,G_r))).





























