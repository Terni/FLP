% ----------- Lambda calcul in PROLOG

% definice
lE(var(_)).
lE(apl(lE(_), lE(_))).
lE(abs(var(_), lE(_))).
 

% Vraci seznam s polozkami bez duplicit
remdub([],[]).
remdub([H|T],TRes):-			myelem(H,T), !, remdub(T,TRes).
remdub([H|T],[H|TRes]):-		remdub(T,TRes).
 
myelem(H,[H|_]):-			!.
myelem(V,[_|T]):-			myelem(V,T).
 

% vraci seznam volnych promenych
volne(E, Res):-				volImpl(E, [], Rs), remdub(Rs, Res).
volImpl(var(X), L, []):- 		myelem(X,L).
volImpl(var(X), L, [X]):- 		not(myelem(X,L)).
volImpl(apl(E1, E2), L, Res):- 		volImpl(E1, L, Res1), volImpl(E2, L, Res2), append(Res1, Res2, Res).
volImpl(abs(var(X), E), L, Res):-	volImpl(E, [X|L], Res).
 

% vraci seznam vazanych promenych
vazane(E, Res):- 			azImpl(E, [], Rs), remdub(Rs, Res).
vazImpl(var(X), L, [X]):- 		myelem(X,L).
vazImpl(var(X), L, []):- 		not(myelem(X,L)).
vazImpl(apl(E1, E2), L, Res):- 		vazImpl(E1, L, Res1), vazImpl(E2, L, Res2), append(Res1, Res2, Res).
vazImpl(abs(var(X), E), L, [X|Res]):- 	vazImpl(E, [X|L], Res).
 
% platnost substituce 
isvalid(E, X, V):-			volne(X,Vol), vazane(X,Vaz), testval(Vol, Vaz, E, V), !.
testval(Vol, Vaz, _, V):- 		not(member(V,Vol)), not(member(V,Vaz)), !.
testval(_, Vaz, _, V):- 		member(V, Vaz), !, fail.
testval(Vol, Vaz, E, V):- 		member(V,Vol), not(member(E,Vaz)), !.