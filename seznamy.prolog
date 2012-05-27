delete(_, [], []).									% odstraneni prvku ze seznamu
delete(X, [X|L], M) :- 			!, delete(X, L, M).				% bez ! bychom pri navraceni ziskavali seznamy,
delete(X, [Y|L1], [Y|L2]) :- 		delete(X, L1, L2).				% kde nejsou vsechny vyskyty X zcela odstraneny


remove(A,[A|L], L) :- 			!. 						% odstraneni prvku ze seznamu
remove(A,[B|L],[B|M]) :- 		remove(A,L,M). 
remove(_,[],[]). 

intersection([], X, []). 								% prunik dvou mnozin
intersection([X|R], Y, [X|Z]) :- 	member(X,Y), !, intersection(R,Y, Z). 		% bez ! by vysledna mnozina pri navraceni postupne
intersection([X|R], Y, Z) :- 		intersection(R, Y, Z). 				% ztracela cleny az do prazdna

union([],X,X) :- !. % sjednoceni dvou mnozin
union([X|R],Y,Z) :- member(X,Y),!,union(R,Y,Z). 
union([X|R],Y,[X|Z]) :- union(R,Y,Z). 

range(S, S, [S]) :- !. 									% generator posloupnosti cisel od S do E
range(S, E, [S|T]) :- 									% ! zastavuje rekurzivni vypocet
      S < E, SS is S+1, 
      range(SS, E, T), !. 
range(_, _, []). 

take(_, [], []) :- !. 									% vrati prvnich N prvku seznamu
take(N, [H|T], [H|TT]) :- 
      N > 0, 
      !, 										% bez ! bychom meli vzdy dale mensi pocet prvku
      NN is N-1, 									% v ruznych kombinacich
      take(NN, T, TT). 
take(N, [_|_], []) :- 
      N =< 0. 




takeWhile(_,[],[]).									% vrati prvnich N prvku seznamu dokud plati dana funkce
takeWhile(P,[H|T],[H|TT]) :-
      PP =.. [P,H], call(PP), !, takeWhile(P,T,TT).
takeWhile(_,_,[]).

?- takeWhile(odd,[1,3,5,6,7,9],L). % uziti predikatu takeWhile, L = [1,3,5,7].

dropWhile(_,[],[]).									% vrati poslednich N prvku jakmile prestane platit funkce
dropWhile(P,[H|T],TT) :-
      PP =.. [P,H], call(PP), !, dropWhile(P,T,TT).
dropWhile(_,L,L).

?- dropWhile(odd,[1,3,4,5,7,9],R).							% uziti predikatu dropWhile, L = [4,5,7,9].


less_than_3(X) :- X<3.									% test < 3 (pro nasled. priklad)


split(_,[],([],[])) :- !.								% rozdeli seznam kdyz funkce prestane platit
split(P,L,R) :- 				split(P,L,[],R).
split(_,[],W,(RW,[])) :- 			reverse(W,RW).
split(P,[H|T],W,R) :-
      PP =.. [P,H], call(PP), !, split(P,T,[H|W],R).
split(_,R,L,(RL,R)) :- 				reverse(L,RL).

?- split(less_than_3,[1,2,3,4,5,6],L).							% uziti predikatu split, L=([1,2],[3,4,5,6]).


map(_, [], []). % predikat map
map(F, [H|T], [NH|NT]) :- P =.. [F,H,NH], 
			  call(P), 
			  map(F,T,NT). 

inc(X,Y) :- var(Y), Y is X+1, !. 							% inkrement (pro nasled. priklad)
inc(X,Y) :- nonvar(Y), Z is Y-1, Z=X. 

?- map(inc,[1,2,3],X). % pouziti map, vysledek [2,3,4]
?- map(inc,X,[2,3,4]). % pouziti map, vysledek [1,2,3]

filter(_, [], []) :- 				!.					% predikat filter
filter(P, [H|T], [H|TT]) :-			PP =.. [P,H], call(PP),			% zde by mohlo byt i zkracene jen: call(P, [H])
						!, filter(P, T, TT).			% bez ! bychom pri navraceni postupne dostavali
filter(P, [_|T], TT) :-				filter(P, T, TT).			% seznam s mensim a mensim poctem spravnych prvku


even(V) :- X is V mod 2, X = 0.								% test sudych cisel
odd(X) :- 										% test lichych cisel
	  Y is X // 2, 
	  YY is Y * 2, 
	  YY \= X. 

?- filter(even,[1,2,3,4,5,6],R). % pouziti filter, vysledek [2,4,6]

foldr(_, B, [], B). 									% predikat foldr
foldr(F, B, [H|T], BB) :- 
	  foldr(F, B, T, BT), 
	  P =.. [F,H,BT,BB], call(P). 
foldl(_, A, [], A). 									% predikat foldl
foldl(F, A, [H|T], AA) :- 
	  P =.. [F,A,H,AT], call(P), 
	  foldl(F, AT, T, AA). 

add(X,Y,Z) :- ZZ is Y+X, ZZ=Z. 								% soucet (pro nasled. priklad)

conS(T,H,[H|T]). 									% prohozeni (pro nasled. priklad)

sum(L, S) :- foldr(add, 0, L, S). 							% pouziti foldr, suma seznamu

rev(L, RL) :- foldl(conS, [], L, RL). 							% pouziti foldl, reverzace seznamu




