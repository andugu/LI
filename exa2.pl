nat(0).
nat(N):- nat(N1), N is N1 + 1.

camino(E, E, C, C).
camino(EstadoActual, EstadoFinal, CaminoHastaAhora, CaminoTotal) :- unPaso( EstadoActual, EstSiguiente ), \+member(EstSiguiente,CaminoHastaAhora), camino( EstSiguiente, EstadoFinal, [EstSiguiente|CaminoHastaAhora], CaminoTotal ).

solucionOptima:-	nat(N),
					camino( [3,3,1], [0,0,0], [ [3,3,1] ], C),
					length(C, N),
					write(C).

unPaso(EA, ES) :- 	action(A),
					precondition(A, EA),
					efect(A, EA, ES),
					safe(ES).

% Accions
action(move(M, C, Dir)) :-
	member([M, C], [[2,0], [0,2], [1,0], [0,1], [1,1]]),
	member(Dir, [left, rigth]).

% Precondicions
precondition(move(Mb, Cb, left), [M,C,0]) :- M1 is 3-M, M1 >= Mb, C1 is 3-C, C1 >= Cb.  
precondition(move(Mb, Cb, rigth), [M,C,1]) :- M >= Mb, C >= Cb.

% Efectes
efect(move(M,C, left), [MA,CA,0], [MD,CD,1]) :- MD is MA + M, CD is CA + C.
efect(move(M,C, rigth), [MA,CA,1], [MD,CD,0]) :- MD is MA - M, CD is CA - C.

% Safe
safe([M,M,_]). % Son iguals
safe([0,_,_]). % Tots a un costat
safe([3,_,_]).
% M >= C and
% 3-M >= 3-C  ==>  C >= M