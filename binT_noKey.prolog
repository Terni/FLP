% ------- BINARNI STORM in PROLOG without a key

empty_tree(empty). % prazdny strom

%%% Pridani noveho uzlu (node) %%%
% V = value, vstupni_Strom, vystupni_Strom
add2tree(V,    empty, node(V, empty, empty)). 						% pro prazdny
add2tree(V,    node(V,L,R), node(V,L,R)) :- 	!. 					% pridani stejny hodnoty
add2tree(Vnew, node(V,L,R), node(V,LL,R)) :-	Vnew < V, !, add2tree(Vnew, L, LL).	% pridani do prava
add2tree(Vnew, node(V,L,R), node(V,L,RR)) :-	add2tree(Vnew, RR, R).

%%% Vytvoreni stromu ze seznamu %%%
list2tree([], empty). 											% prazdny seznam = prazdny strom
list2tree([X|XS],NewBTree) :- 			list2tree(XS, BTree), add2tree(X, BTree, NewBTree).	% postupne vytvoreni ze seznamu
% list2tree([X|XS],NewBTree) :- 			add2tree(X, BTree, NewBTree), list2tree(XS, BTree).	% postupne vytvoreni ze seznamu
% ?- list2tree([1,2,3,4,5], BT).

%%% D = deep %%%
deeptree(empty,0).
deeptree(node(_,L,R),D) :- 		deeptree(L,LD), deeptree(R,RD), max(LD,RD,MAXI), D is MAXI+1.

max(L,R,Maximum):- (L>R -> Maximum is L ; Maximum is R).
% deeptree(node(1, empty, node(2, empty, empty))).
%?- deeptree( node(2, node(1,empty,empty), node(4,node(3,empty,empty),empty) ), D). % pouziti predikatu hloubkaStromu

preOrder(empty, []).
preOrder(node(V,L,R), List) :-			preOrder(L, LL), preOrder(R, RR), append([V|LL], RR, List).

inOrder(empty, []).
inOrder(node(V,L,R), List) :-			inOrder(L, LL), inOrder(R, RR), append(LL, [V|RR], List).

posOrder(empty, []).
posOrder(node(V,L,R), List) :-			posOrder(L, LL), posOrder(R, RR), append([LL,RR], [V], List).

% posOrder( node(0, node(-1, node(-2, empty, empty), empty), node(1, empty, node(2, empty, empty))), L).

%%% hleda hodnotu Vin vrati ji nebo False kdyz nenajde %%%
searchValue(empty, _, _) :- 			!, fail.
searchValue(node(V,_,_), Vin, Vout):-		Vin == V,Vout is V,!.
searchValue(node(V,L,_), Vin, Vout):-		Vin < V, !, searchValue(L, Vin, Vout).
searchValue(node(_,L,R), Vin, Vout):-		searchValue(L, Vin, Vout);searchValue(R, Vin, Vout).
%pouziti: searchValue(node(0, node(-3, node(-1,empty,empty), node(-2,empty,empty)), node(1, node(2,empty,empty), node(3,empty,empty))),-3, V).



% ---------------------------------------------------------------------------------------------------
mensi_jak_3(P):- P < 3. % definovanej valstni predikat

subTreeLeft(empty, empty).								% hlavni, a hledany prazdny
subTreeLeft(node(_,_,_), empty) :- 		!, fail.				% hledany je prazdny
subTreeLeft(empty,node(_,_,_)).								% hlavni je prazdny
subTreeLeft(node(V,L,R),node(V,LL,RR)) :-  	subTreeLeft(L,LL),  subTreeLeft(R,RR).	% hedani v levo a v pravo

% najde substuvatelny storm: hledany, hlavni
findSubT(T,node(V,L,R)) :-			subTreeLeft(T,node(V,L,R)),!.		% kdyz jsou stejne
findSubT(T,node(_,L,_)) :-			subTreeLeft(T,L),!.			% jdu doleva
findSubT(T,node(_,_,R)) :-			subTreeLeft(T,R),!.			% jdu doprava
findSubT(empty,empty).									% prazdny
%pouziti: findSubT(node(1,empty,empty), node(0, node(-1, node(-2,empty,empty), empty), node(1, empty, node(2,empty,empty)))).

psub(_,_,empty,empty).
psub(P,W,node(V,L,R),node(W,LL,RR)) :- 		F =.. [P,V], call(F),!,  psub(P,W,L,LL),  psub(P,W,R,RR).
psub(P,W,node(V,L,R),node(V,LL,RR)) :-         				 psub(P,W,L,LL),  psub(P,W,R,RR).
% F =.. [P,V] odpovida P(V), P je pozice ve strome

%% P je prediaktor pro urceni jestli je to mensi jak tri ta hodnota tak ji vsude nahradim
subsIFsubt(ST,T,P,W,NT) :-			findSubT(ST,T),!,        psub(P,W,T,NT).
subsIFsubt(_,T,_,_,T).
% % pouziti: subsIFsubt(node(1,node(0,empty,empty),node(2,empty,empty)),node(5,node(1,node(0,empty,empty),node(2,empty,empty)),node(8,empty,empty)),mensi_jak_3,0,NT).


% ---------------------------------------------------------------------------------------------------
insval(V, empty, node(V, empty, empty)).					% pro prazdne hodnoty
insval(V, node(W, L, R), node(W, LL, R)) :-	V < W, insval(V, L, LL).	% kontrola vlevo hodnoty
insval(V, node(W, L, R),node(W, L, RR)) :-	V > W, insval(V, R, RR).	% kontrola pravo hodnoty
insval(V, node(V, L, R), node(V, L, R)).
 
l2t([], empty).	% list2tree
l2t([H|T], R) :-
	l2t(T, TR),
	insval(H, TR, R).
 
t2l(empty, []). % tree2list
t2l(node(V, L, R), ResL) :-
	t2l(L, LL),
	t2l(R, RL),
	append(LL, [V|RL], ResL).

% strom do listu ale postupne
tree2list(T, L) :-	var(T), var(L), !, fail.
tree2list(T, L) :-	var(T), l2t(T, L).
tree2list(T, L) :-	var(L), t2l(T, L).
tree2list(T, L) :-	nonvar(L), nonvar(T), t2l(T, LL), LL = L. /* tady to slo ruzne, nic neni 100% */
% pouziti:
% tree2list(node(0, node(-3, node(-1,empty,empty), node(-2,empty,empty)), node(1, node(2,empty,empty), node(3,empty,empty))), L).
% L = [-1, -3, -2, 0, 2, 1, 3] .

% predikat=(funkce): var() a nonvar() rikaji jestli je volna nebo neni volny atom=(promena)