% Exercici B:
programa(L) :- programa(L, []).
programa(L0, L3) :- beginT(L0, L1), instrucciones(L1, L2), endT(L2, L3).

instrucciones(L0, L1) :- instruccion(L0, L1).
instrucciones(L0, L3) :- instruccion(L0, L1), semiColT(L1, L2) , instrucciones(L2, L3).

instruccion(L0, L5) :- variable(L0, L1), eqT(L1, L2), variable(L2, L3), sumT(L3, L4), variable(L4, L5).
instruccion(L0, L9) :- ifT(L0, L1), variable(L1, L2), eqT(L2, L3), variable(L3, L4), thenT(L4, L5), instrucciones(L5, L6), elseT(L6, L7), instrucciones(L7, L8), endifT(L8, L9).

% Variables
variable([x|T], T).
variable([y|T], T).
variable([z|T], T).

% Terminals
beginT([begin|T], T).
endT([end|T], T).
semiColT([;|T], T).
eqT([=|T], T).
sumT([+|T], T).
ifT([if|T], T).
thenT([then|T], T).
elseT([else|T], T).
endifT([endif|T], T).