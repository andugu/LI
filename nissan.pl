:-dynamic(varNumber / 3).
symbolicOutput(0).  % set to 1 to see symbolic output only; 0 otherwise.

% Nissan needs to define an "inter-factory" committee.  Each factory
% proposes a list of possible representatives among a set of numPeople
% persons (see the input below). For each factory, the committee must
% include at least one of its proposed representatives.  The committee
% can have at most maxMembers members (see below), but our aim is to
% further reduce this: to find an as small as possible committee.
% Solve this with SAT, completing and delivering this file named
% "nissan.pl".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% To use this prolog template for optimization problems, complete the code parts 1,2,3,4 below.    %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Example input:

numPeople(35).
maxMembers(5).

factory(1,[8,35,18,16,23,7,19,34,22,24,4,26,12,31,29,28,30,27,2]).
factory(2,[28,27,35,17,5,19,22,12,16,6,15,23,18,20,2,3,8,10,1,25,32]).
factory(3,[35,17,24,15,22,2,21,7,1,27,5,13,33,9,31,16,12,28,3,34,20,4,18]).
factory(4,[11,21,31,12,10,28,6,20,17,13,23,30,24,35,16,25,18,3,5,27,1,26]).
factory(5,[28,32,35,4,12,26,19,13,2,15,21,1,30,3,31,20,33,7,17,34,24]).
factory(6,[11,4,28,33,10,9,5,23,32,20,21,24,8,3,19,18,12,16,34]).
factory(7,[8,17,15,6,34,9,29,21,14,20,7,22,12,11,5,2,19,35,16]).
factory(8,[17,19,7,5,14,13,18,6,4,21,2,28,3,16,22,31,23,25,1]).
factory(9,[33,8,16,27,28,18,20,22,30,26,34,23,4,25,29,2,17,24]).
factory(10,[14,27,12,34,16,28,31,3,19,26,8,32,11,9,5,18,30,4,2,23,33]).
factory(11,[28,24,27,21,25,20,29,2,34,35,17,9,5,11,32,26,15,18,13,6]).
factory(12,[11,12,25,3,18,1,14,20,2,9,15,33,31,17,32,16,24,35,28,34,8,27]).
factory(13,[32,23,14,26,6,24,18,11,8,2,1,13,21,5,34,33,10,27,16,28]).
factory(14,[26,3,10,22,9,11,29,34,27,21,32,33,7,6,1,5,2]).
factory(15,[8,16,1,32,11,30,20,34,23,4,5,27,24,33,14,28,15,18,26,7,35]).
factory(16,[11,2,22,20,3,30,35,13,33,18,9,29,15,5,32,26,10,34,1,31]).
factory(17,[13,8,5,18,10,12,28,15,9,29,1,35,33,3,32,19,26,17]).
factory(18,[26,23,33,2,16,30,6,1,12,3,13,11,29,20,34,28,27,14,7,19,5,9,24,17]).
factory(19,[26,15,2,11,17,7,33,12,21,18,27,13,35,25,9,34,23,3,28,10]).
factory(20,[16,14,6,2,11,30,12,32,1,13,20,28,19,29,31,17,33,22,25,4,5,27,7,18]).
factory(21,[12,26,13,33,11,7,30,4,6,8,24,25,21,1,20,9,14,17,19]).
factory(22,[21,10,17,28,4,11,23,24,14,2,29,25,20,27,19,6]).
factory(23,[30,8,20,13,22,12,7,31,33,6,29,26,27,2,4,11,23,17,15,35]).
factory(24,[29,16,6,24,23,22,25,4,31,26,21,33,20,5,1,2,7,9,19,30,35,28]).
factory(25,[17,6,33,8,9,26,25,29,4,10,35,1,3,2,16,27,34,11,5,22,21,24]).
factory(26,[8,25,31,34,32,6,4,22,10,2,21,23,3,29,24,19,14,28,30,16]).
factory(27,[19,30,2,22,1,14,13,6,16,27,15,26,23,18,29,3,12,11,33,7,5]).
factory(28,[14,13,23,25,12,1,16,8,17,21,26,31,34,27,7,33,24,20,28,22]).
factory(29,[9,8,25,21,16,13,23,15,14,28,35,30,31,29,17,32,26,12,19,3,22]).
factory(30,[15,5,17,1,2,21,9,26,20,29,19,28,18,16,32,25,35,8,24,4]).
factory(31,[11,24,31,34,4,16,9,7,18,30,29,6,22,3,33,28,12,21,1,19,26,27]).
factory(32,[5,13,21,24,22,19,31,29,2,7,20,9,17,12,34,32,26,23,27]).
factory(33,[26,15,16,35,14,30,7,4,17,6,11,19,3,2,5,18]).
factory(34,[32,17,1,4,29,33,13,20,24,35,9,12,26,3,6,30,34,27,14,2,5,18,11]).
factory(35,[33,18,5,7,15,11,29,16,3,1,8,28,4,10,14,32,35,31,34,9]).
factory(36,[34,9,7,30,12,3,16,26,6,31,8,35,24,28,10,4,15,20,22,5]).
factory(37,[5,14,20,33,18,35,28,26,2,12,30,3,10,17,34,6,29,22,27,19,8]).
factory(38,[18,34,25,9,15,26,1,5,11,4,6,7,12,30,32,27,23,19,22,31,17]).
factory(39,[19,30,18,3,7,26,23,20,17,32,11,14,6,24,12,4]).
factory(40,[8,19,30,13,28,5,4,1,18,16,12,9,21,14,27,2,7,25,23,22,6,26]).
factory(41,[20,6,27,23,17,18,34,7,3,1,4,21,28,22,31,24,16,33,8]).
factory(42,[14,27,15,18,2,10,30,3,33,31,32,23,20,5,11,13,29]).
factory(43,[22,10,7,3,14,29,5,1,27,17,28,9,6,15,34,12,32,25,16,30]).
factory(44,[7,23,5,27,9,21,26,10,11,25,29,16,13,22,14,20,34,17,28,18,4]).
factory(45,[4,6,19,16,31,5,23,24,34,35,20,13,11,12,29,15,25,28,30]).
factory(46,[31,20,25,27,14,16,28,7,10,22,4,8,19,12,29,23,32,1,3]).
factory(47,[3,14,6,31,33,16,7,22,27,30,19,34,17,29,21,32]).
factory(48,[10,25,28,29,15,33,20,17,9,7,11,27,24,26,8,4,22,34,21,14]).
factory(49,[13,4,16,19,12,7,32,34,22,31,15,30,2,6,25,20,27,21,11,9,35]).
factory(50,[30,1,31,29,33,32,25,11,27,16,34,4,3,15,20,10,5,23,14,18]).
factory(51,[2,3,18,15,30,32,33,9,24,26,10,29,27,20,6,35,12,34]).
factory(52,[6,27,18,13,2,25,1,4,7,20,35,17,3,19,32,15,26,34,9]).
factory(53,[34,32,15,10,3,29,4,2,11,13,26,6,5,31,1,18,14,21]).
factory(54,[13,18,10,7,31,5,25,23,3,21,1,29,24,6,15,26,27,32,20,33,14]).
factory(55,[18,33,31,35,1,26,30,22,23,13,24,7,34,12,14,28,20,27,11,21,3,29]).
factory(56,[21,3,34,12,22,16,27,6,23,25,26,31,17,11,4,8,29,15,18]).
factory(57,[34,12,31,1,7,10,13,35,20,23,32,14,9,30,25,27,18,3,19,33,2]).
factory(58,[4,13,12,20,14,19,15,27,16,10,31,7,34,5,21,11,8,30,33]).
factory(59,[35,28,14,29,9,23,6,15,18,12,10,16,32,1,27,2,19,11,25,21]).
factory(60,[26,7,6,23,35,12,9,21,3,19,5,2,16,32,33,22,17,8,13]).


%%%%%% Some helpful definitions to make the code cleaner:
person(P):- numPeople(N), between(1, N, P).

% 1.- Declare SAT variables to be used
% c(P) means person(P) is in the "inter-factory" committee.
satVariable( c(P) ):- person(P).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 2. This predicate writeClauses(MaxCost) generates the clauses that guarantee that
% a solution with cost at most MaxCost is found

writeClauses(infinite):- !, maxMembers(N), writeClauses(N), !.  %initially, the size can be as large as N
writeClauses(MaxSize):-
    atLeastOneFromEachFactory,
    atMostMaxMembers(MaxSize),
    true,!.
writeClauses(_):- told, nl, write('writeClauses failed!'), nl,nl, halt.

atLeastOneFromEachFactory :- factory(_, L), findall(c(P), member(P, L), Lits), atLeast(1, Lits), fail.
atLeastOneFromEachFactory.

atMostMaxMembers(MaxSize) :- findall(c(P), person(P), Lits), atMost(MaxSize, Lits), fail.
atMostMaxMembers(_).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 3. This predicate displays a given solution M:

% This must write solutions in a format like this one: "Solution of cost 5:  [2,33,9,11,14]"

displaySol(M):- costOfThisSolution(M, N), write('Solution of cost '), write(N), write(':  '),
                findall(P, member(c(P), M), L), write(L).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 4. This predicate computes the cost of a given solution M:

costOfThisSolution(M, Cost):- findall(c(P), member(c(P), M), L), sort(L,L1), length(L1, Cost), !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% No need to modify anything beow this line:

main:-  symbolicOutput(1), !, writeClauses(infinite), halt.   % print the clauses in symbolic form and halt
main:-
    told, write('Looking for initial solution with arbitrary cost...'), nl,
    initClauseGeneration,
    tell(clauses), writeClauses(infinite), told,
    tell(header),  writeHeader,  told,
    numVars(N), numClauses(C), 
    write('Generated '), write(C), write(' clauses over '), write(N), write(' variables. '),nl,
    shell('cat header clauses > infile.cnf',_),
    write('Launching picosat...'), nl,
    shell('picosat -v -o model infile.cnf', Result),  % if sat: Result=10; if unsat: Result=20.
    treatResult(Result,[]),!.

treatResult(20,[]       ):- write('No solution exists.'), nl, halt.
treatResult(20,BestModel):-
    costOfThisSolution(BestModel,Cost), write('Unsatisfiable. So the optimal solution was this one with cost '),
    write(Cost), write(':'), nl, displaySol(BestModel), nl,nl,halt.
treatResult(10,_):- %   shell('cat model',_),
    write('Solution found '), flush_output,
    see(model), symbolicModel(M), seen,
    costOfThisSolution(M,Cost),
    write('with cost '), write(Cost), nl,
    displaySol(M), 
    Cost1 is Cost-1,   nl,nl, write('Now looking for solution with cost '), write(Cost1), write('...'), nl,
    initClauseGeneration, tell(clauses), writeClauses(Cost1), told,
    tell(header),  writeHeader,  told,
    numVars(N),numClauses(C),
    write('Generated '), write(C), write(' clauses over '), write(N), write(' variables. '),nl,
    shell('cat header clauses > infile.cnf',_),
    write('Launching picosat...'), nl,
    shell('picosat -v -o model infile.cnf', Result),  % if sat: Result=10; if unsat: Result=20.
    treatResult(Result,M),!.
treatResult(_,_):- write('cnf input error. Wrote something strange in your cnf?'), nl,nl, halt.
    

initClauseGeneration:-  %initialize all info about variables and clauses:
	retractall(numClauses(   _)),
	retractall(numVars(      _)),
	retractall(varNumber(_,_,_)),
	assert(numClauses( 0 )),
	assert(numVars(    0 )),     !.

writeClause([]):- symbolicOutput(1),!, nl.
writeClause([]):- countClause, write(0), nl.
writeClause([Lit|C]):- w(Lit), writeClause(C),!.
w(-Var):- symbolicOutput(1), satVariable(Var), write(-Var), write(' '),!. 
w( Var):- symbolicOutput(1), satVariable(Var), write( Var), write(' '),!. 
w(-Var):- satVariable(Var),  var2num(Var,N),   write(-), write(N), write(' '),!.
w( Var):- satVariable(Var),  var2num(Var,N),             write(N), write(' '),!.
w( Lit):- told, write('ERROR: generating clause with undeclared variable in literal '), write(Lit), nl,nl, halt.


% given the symbolic variable V, find its variable number N in the SAT solver:
var2num(V,N):- hash_term(V,Key), existsOrCreate(V,Key,N),!.
existsOrCreate(V,Key,N):- varNumber(Key,V,N),!.                            % V already existed with num N
existsOrCreate(V,Key,N):- newVarNumber(N), assert(varNumber(Key,V,N)), !.  % otherwise, introduce new N for V

writeHeader:- numVars(N),numClauses(C), write('p cnf '),write(N), write(' '),write(C),nl.

countClause:-     retract( numClauses(N0) ), N is N0+1, assert( numClauses(N) ),!.
newVarNumber(N):- retract( numVars(   N0) ), N is N0+1, assert(    numVars(N) ),!.

% Getting the symbolic model M from the output file:
symbolicModel(M):- get_code(Char), readWord(Char,W), symbolicModel(M1), addIfPositiveInt(W,M1,M),!.
symbolicModel([]).
addIfPositiveInt(W,L,[Var|L]):- W = [C|_], between(48,57,C), number_codes(N,W), N>0, varNumber(_,Var,N),!.
addIfPositiveInt(_,L,L).
readWord( 99,W):- repeat, get_code(Ch), member(Ch,[-1,10]), !, get_code(Ch1), readWord(Ch1,W),!. % skip line starting w/ c
readWord(115,W):- repeat, get_code(Ch), member(Ch,[-1,10]), !, get_code(Ch1), readWord(Ch1,W),!. % skip line starting w/ s
readWord(-1,_):-!, fail. %end of file
readWord(C,[]):- member(C,[10,32]), !. % newline or white space marks end of word
readWord(Char,[Char|W]):- get_code(Char1), readWord(Char1,W), !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Everything below is given as a standard library, reusable for solving 
%    with SAT many different problems.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Express that Var is equivalent to the disjunction of Lits:
expressOr( Var, Lits) :- symbolicOutput(1), write( Var ), write(' <--> or('), write(Lits), write(')'), nl, !. 
expressOr( Var, Lits ):- member(Lit,Lits), negate(Lit,NLit), writeClause([ NLit, Var ]), fail.
expressOr( Var, Lits ):- negate(Var,NVar), writeClause([ NVar | Lits ]),!.

% Express that Var is equivalent to the conjunction of Lits:
expressAnd( Var, Lits) :- symbolicOutput(1), write( Var ), write(' <--> and('), write(Lits), write(')'), nl, !. 
expressAnd( Var, Lits):- member(Lit,Lits), negate(Var,NVar), writeClause([ NVar, Lit ]), fail.
expressAnd( Var, Lits):- findall(NLit, (member(Lit,Lits), negate(Lit,NLit)), NLits), writeClause([ Var | NLits]), !.


%%%%%% Cardinality constraints on arbitrary sets of literals Lits:

exactly(K,Lits):- symbolicOutput(1), write( exactly(K,Lits) ), nl, !.
exactly(K,Lits):- atLeast(K,Lits), atMost(K,Lits),!.

atMost(K,Lits):- symbolicOutput(1), write( atMost(K,Lits) ), nl, !.
atMost(K,Lits):-   % l1+...+ln <= k:  in all subsets of size k+1, at least one is false:
	negateAll(Lits,NLits),
	K1 is K+1,    subsetOfSize(K1,NLits,Clause), writeClause(Clause),fail.
atMost(_,_).

atLeast(K,Lits):- symbolicOutput(1), write( atLeast(K,Lits) ), nl, !.
atLeast(K,Lits):-  % l1+...+ln >= k: in all subsets of size n-k+1, at least one is true:
	length(Lits,N),
	K1 is N-K+1,  subsetOfSize(K1, Lits,Clause), writeClause(Clause),fail.
atLeast(_,_).

negateAll( [], [] ).
negateAll( [Lit|Lits], [NLit|NLits] ):- negate(Lit,NLit), negateAll( Lits, NLits ),!.

negate( -Var,  Var):-!.
negate(  Var, -Var):-!.

subsetOfSize(0,_,[]):-!.
subsetOfSize(N,[X|L],[X|S]):- N1 is N-1, length(L,Leng), Leng>=N1, subsetOfSize(N1,L,S).
subsetOfSize(N,[_|L],   S ):-            length(L,Leng), Leng>=N,  subsetOfSize( N,L,S).

