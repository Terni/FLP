% ------- BINARNI STORM in PROLOG with a key

empty_tree(leaf). % prazdny strom

%%% pridani stromu %%%
add2tree(K, V, leaf, node(K,V,leaf,leaf)). 
add2tree(K, _, node(K,V,X,Y), node(K,V,X,Y)) :-  	!. 
add2tree(Kn, Vn, node(K,V,L,R), node(K,V,LL,R)) :- 	Kn < K, !, add2tree(Kn, Vn, L, LL). 
add2tree(Kn, Vn, node(K,V,L,R), node(K,V,L,RR)) :- 	add2tree(Kn, Vn, R, RR).
%?- add2tree(6,petr,leaf,R), add2tree(10,jana,R,RR). % uziti predikatu add2tree
%add2tree(1,1,leaf,R), add2tree(2,2,R,RR).

%%% prevod seznamu na strom %%%
list2tree([],leaf). 
list2tree([I|IS], NewTree) :- 
	list2tree(IS, Tree), 
	add2tree(I, I, Tree, NewTree). 
%?- list2tree([1,2,3],T). % uziti predikatu list2tree

%%% D = deep %%%
deeptree(leaf,0).
deeptree(node(_,_,L,R),D) :- 		deeptree(L,LD), deeptree(R,RD), max(LD,RD,MAXI), D is MAXI+1.

max(L,R,Maximum):- (L>R -> Maximum is L ; Maximum is R).
% deeptree(node(1, 1, leaf, node(2, 2, leaf, leaf))).
%?- deeptree( node(2,2, node(1,1,leaf,leaf), node(4,4,node(3,3,leaf,leaf),leaf) ), D). % pouziti predikatu hloubkaStromu

%%%% preOrder pruchod stromem %%%
preOrder(leaf, []). 
preOrder(node(_,Value,L,R), List) :- 
	preOrder(L, LL), 
	preOrder(R, RL), 
	append([Value|LL], RL, List). 
%?- preOrder(Tree, List). % uziti predikatu preOrder


%%%% inOrder pruchod stromem %%%
inOrder(leaf, []). 
inOrder(node(_,Value,L,R), List) :- 
	inOrder(L, LL), 
	inOrder(R, RL), 
	append(LL, [Value|RL], List). 
%?- inOrder(Tree, List). % uziti predikatu inOrder


%%% % posOrder pruchod stromem %%%
posOrder(leaf, []).
posOrder(node(_,Value,[],R), List) :- 		posOrder(R, RL), append([RL], [Value], List). % vetev jen skrz spravneho vypisu
posOrder(node(_,Value,L,R), List) :- 
	posOrder(L, LL), 
	posOrder(R, RL), 
	append([LL|RL], [Value], List). 
%?- posOrder(Tree, List). % uziti predikatu posOrder


%%% vyhledani hodnoty podle klice %%%
search(_, leaf, _) :- 				!, fail. 
search(Key, node(Key,Value,_,_), Value) :- 	!. 
search(Key, node(KeyT,_,L,_), Value) :- 	Key < KeyT, !, search(Key, L, Value). 
search(Key, node(_,_,_,R), Value) :- 		search(Key, R, Value). 
%?- search(6, Tree, Data). % uziti predikatu search
