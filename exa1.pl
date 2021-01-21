nat(0).
nat(N):- nat(N1), N is N1 + 1.

camino(E, E, C, C).
camino(EstadoActual, EstadoFinal, CaminoHastaAhora, CaminoTotal) :- unPaso( EstadoActual, EstSiguiente ), \+member(EstSiguiente,CaminoHastaAhora), camino( EstSiguiente, EstadoFinal, [EstSiguiente|CaminoHastaAhora], CaminoTotal ).

solucionOptima:-	nat(N),								% Buscamos solución de "coste" 0; si no, de 1, etc.
					camino([0,0],[0,4],[[0,0]],C), 		% En "hacer aguas": -un estado es [cubo5,cubo8], y
					length(C,N),						% -el coste es la longitud de C.
					write(C).

unPaso(EA, ES) :- 	action(A),
					precondition(A, EA),
					efect(A, EA, ES).

% Accions
action(omplir1).
action(omplir2).
action(buidar1).
action(buidar2).
action(pasar1a2).
action(pasar2a1).

% Precondicions
precondition(omplir1, [C1, _]) :- C1 < 5.
precondition(omplir2, [_, C2]) :- C2 < 8.
precondition(buidar1, [C1, _]) :- C1 > 0.
precondition(buidar2, [_, C2]) :- C2 > 0.
precondition(pasar1a2, [C1, C2]) :- C1 > 0, C2 < 8.
precondition(pasar2a1, [C1, C2]) :- C2 > 0, C1 < 5.

% Efectes
efect(omplir1, [C1, C2], [5, C2]).
efect(omplir2, [C1, C2], [C1, 8]).
efect(buidar1, [C1, C2], [0, C2]).
efect(buidar2, [C1, C2], [C1, 0]).
efect(pasar1a2, [C1, C2], [R, 8]) :- 	T is C1+C2, T > 8, !,
										R is T - 8.
efect(pasar1a2, [C1, C2], [0, T]) :- 	T is C1+C2.

efect(pasar2a1, [C1, C2], [5, R]) :- 	T is C1+C2, T > 5, !,
										R is T - 5.
efect(pasar2a1, [C1, C2], [T, 0]) :- 	T is C1+C2.
