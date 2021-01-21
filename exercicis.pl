% Basics

pert(X,[X|_]).
pert(X,[_|L]):- pert(X,L).

concat([],L,L).
concat([X|L1],L2,[X|L3]):- concat(L1,L2,L3).

fact(0,1):-!.
fact(X,F):-  X1 is X - 1, fact(X1,F1), F is X * F1.

nat(0).
nat(N):- nat(N1), N is N1 + 1.

pert_con_resto(X,L,R) :- concat(L1,[X|L2],L), concat(L1,L2,R).

permutacion([],[]).
permutacion(L,[X|P]) :- pert_con_resto(X,L,R), permutacion(R,P).

unpaso(A+B,A+C):- unpaso(B,C),!.
unpaso(B+A,C+A):- unpaso(B,C),!.
unpaso(A*B,A*C):- unpaso(B,C),!.
unpaso(B*A,C*A):- unpaso(B,C),!.
unpaso(0*_,0):-!.
unpaso(_*0,0):-!.
unpaso(1*X,X):-!.
unpaso(X*1,X):-!.
unpaso(0+X,X):-!.
unpaso(X+0,X):-!.
unpaso(N1+N2,N3):- number(N1), number(N2), N3 is N1+N2,!.
unpaso(N1*N2,N3):- number(N1), number(N2), N3 is N1*N2,!.
unpaso(N1*X+N2*X,N3*X):- number(N1), number(N2), N3 is N1+N2,!.
unpaso(N1*X+X*N2,N3*X):- number(N1), number(N2), N3 is N1+N2,!.
unpaso(X*N1+N2*X,N3*X):- number(N1), number(N2), N3 is N1+N2,!.
unpaso(X*N1+X*N2,N3*X):- number(N1), number(N2), N3 is N1+N2,!.

% Exercici 2

prod([], 0).
prod([X], X).
prod([X|L], P) :- prod(L, P2), P is X * P2.

% Exercici 3

pescalar([], [], 0).
pescalar([X|L1], [Y|L2], P) :- pescalar(L1, L2, P2), P is P2+X*Y.

% Exercici 4

unio([], X, X).
unio([X|C1], C2, C3) :- pert(X, C2), !, unio(C1, C2, C3).
unio([X|C1], C2, [X|C3]) :- unio(C1, C2, C3).

interseccio([], _, []).
interseccio([X|C1], C2, [X|C3]) :- pert(X, C2), !, interseccio(C1, C2, C3).
interseccio([_|C1], C2, C3) :- interseccio(C1, C2, C3).

% Exercici 5

ultim(L, X) :- concat(_, [X], L).

invers([], []).
invers(L, [X|L2]) :- concat(L1, [X], L), invers(L1, L2).

% Exercici 6

fib(1, 1).
fib(2, 1).
fib(N, F) :- X1 is N-1, X2 is N-2, fib(X1, F1), fib(X2, F2), F is F1+F2.

% Exercici 7

dados(0, 0, []).
dados(P, N, [X|L]) :- N > 0, pert(X, [1, 2, 3, 4, 5, 6]), P1 is P-X, N1 is N-1, dados(P1, N1, L).

% Exercici 8

suma([], 0).
suma([X|L], P) :- suma(L, P2), P is X + P2.

suma_demas(L) :- pert_con_resto(X, L, R), suma(R, X).

% Exercici 9

suma_ants(L) :- concat(A, [X|_], L), suma(A, X).

% Exercici 10

card2([], []).
card2([X|L], [[X, N2] | T]) :- card2(L, C), pert_con_resto([X, N], C, T), !, N2 is N+1.
card2([X|L], [[X, 1] | C]) :- card2(L, C).
card(L) :- card2(L, C), write(C).

% Exercici 11

esta_ordenada([X], X).
esta_ordenada([X|L], X) :- esta_ordenada(L, P2), P2 >= X.
esta_ordenada(L) :- esta_ordenada(L, P).

% Exercici 12

ordenacion(L1, L2):- permutacion(L1, L2), esta_ordenada(L2).

% Exercici 13: Com que esta_ordenada(L2) fa n-1 comparacions, i permutacion fa fins a n!
% combinacions diferents. Per tant tenim n!*(n-1) que es aproximadament de n!*n

% Exercici 14

insercion(X, [], [X]).
insercion(X, [Y|L1], [X, Y|L1]) :- Y >= X.
insercion(X, [Y|L1], [Y|L2]) :- insercion(X, L1, L2).

ordenacion_insert([],[]).
ordenacion_insert([X|L1], L2) :- ordenacion_insert(L1, L3), insercion(X, L3, L2).

% Exercici 15: insercion() fa en el pitjor cas n comparacions, pero tenint en compte que
% la llista disminueix amb crida, podem dir que el cost mig sera de n/2. Ordenacion passa pels n numeros
% de la llista, per tant n*(n/2).

% Exercici 16: 

meitat([], [], []). % esq input, mig i dreta output
meitat([X], [X], []).
meitat([X, Y|L], [X|L1], [Y|L2]) :- meitat(L, L1, L2).

merge([], [], []).
merge([X], [], [X]).
merge([], [X], [X]).
merge([X|L1], [Y|L2], [X|R]) :- X =< Y, merge(L1, [Y|L2], R).
merge([X|L1], [Y|L2], [Y|R]) :- Y =< X, merge([X|L1], L2, R).

ordenacion_merge([], []).
ordenacion_merge([X], [X]).
ordenacion_merge(L, R) :- meitat(L, L1, L2), ordenacion_merge(L1, R1), ordenacion_merge(L2, R2), merge(R1, R2, R).

% Exercici 17

nmembers(_, 0, []) :- !.
nmembers(A, N, [X|S]) :- pert(X, A), N1 is N-1, nmembers(A, N1, S).

escriu([]) :- write(' '), nl, !.
escriu([X|L]) :- write(X), escriu(L).

diccionario(A, N) :- nmembers(A, N, P), escriu(P), fail.

% Exercici 18

es_palindrom([]).
es_palindrom([_]) :- !.
es_palindrom([X|L]) :- concat(L1, [X], L), es_palindrom(L1).

% setof == setof(Input, (Condicions), Output) a Output hi ha els inputs que compleixen un sol cop
palindromos(L) :- setof(R1, (permutacion(L, R1), es_palindrom(R1)), S), write(S).

% Exercici 19

%Retorna ture si L1+L2 == L3
suma_listas(L1, L2, L3) :- suma_listas(L1, L2, L3, R1, R2), R1 == R2.

suma_listas([], [], [], 0, 0).
suma_listas([], [], [Z], 0, Z).
suma_listas([X|L1], [Y|L2], [Z|L3], R1, R2) :- suma_listas(L1, L2, L3, T1, T2), R1 is (T1*10)+X+Y, R2 is (T2*10)+Z.

busca_permutacio :- L = [S, E, N, D, M, O, R, Y, _, _],
         permutacion(L, [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]),
         suma_listas([D, N, E, S], [E, R, O, M], [Y, E, N, O, M]),
         write('S = '), write(S), nl,
		 write('E = '), write(E), nl,
		 write('N = '), write(N), nl,
		 write('D = '), write(D), nl,
		 write('M = '), write(M), nl,
		 write('O = '), write(O), nl,
		 write('R = '), write(R), nl,
		 write('Y = '), write(Y), nl,
		 write('SEND = '), write([S,E,N,D]), nl,
		 write('MORE = '), write([M,O,R,E]), nl,
		 write('SEND + MORE = '), write([S+M, E+O, N+R, D+E]), nl,
		 write('---------------------------'), nl,
	     write('MONEY = '), write([M,O,N,E,Y]), nl.

% Exercici 20

simplifica(X, L) :- unpaso(X, X1), !, simplifica(X1, L).
simplifica(X, X).

% Exercici 21

mis:- camino( [lado1,3,3], [lado2,0,0], [[lado1,3,3]] ).

camino(Fin,Fin,Cam):- inverso(Cam,Sol), write(Sol), nl.
camino(Ini,Fin,Cam):- paso(Ini,E), novisitado(E,Cam), camino(E,Fin,[E|Cam]).

novisitado(E,Cam):- pert(E,Cam), !,fail.
novisitado(_,_).

paso( [lado1,M1,C1], [lado2,M2,C2] ):- pasan(M,C), M2 is M1-M, C2 is C1-C, safe(M2,C2).
paso( [lado2,M1,C1], [lado1,M2,C2] ):- pasan(M,C), M2 is M1+M, C2 is C1+C, safe(M2,C2).

pasan(M,C):- member( [M,C], [ [0,1], [0,2], [1,0], [1,1], [2,0] ] ).

safe(M,C):- M>=0, M=<3, C>=0, C=<3, nocomen( M, C),
            M1 is 3-M,  C1 is 3-C,  nocomen(M1,C1).

nocomen(0,_).
nocomen(M,C):- M>=C.