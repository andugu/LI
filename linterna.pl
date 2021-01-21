%  Four people need to cross a bridge that can only carry the weight
%  of two of them at the same time.  Moreover, it is a dark night and
%  they have only one flashlight, which is necessary (so somebody has
%  to return with the flashlight if more people remain to cross).
%  When two people cross together, they go at the speed of the
%  slowest one.  The four people need 1,2,5 and 8 minutes respectively
%  to cross.

%  Complete this code to determine how many minutes are really needed to get
%  all four to the other side and how to do that.
%  Note: By hand one can easily find a solution taking 17 minutes; but less...

% A State consists of:  [ sideOfFlashLight, PeopleOnTheLeft, PeopleOnTheRight ].
% And we want to go from initialState to finalState:
initialState( [ left,  [1,2,5,8],  []         ] ).
finalState(   [ right, [],         [1,2,5,8]  ] ).


optimalSolution :-
    initialState(InitialState),
    finalState(FinalState),
    between(1, 100, Minutes),   % We look for a solution taking 1 minute; otherwise, 2 minutes, etc.
    path( Minutes, InitialState, FinalState, [InitialState], Path ),
    reverse(Path,Path1),
    write(Path1), write(' taking '), write(Minutes), write(' minutes.'), nl, halt.

% path( Minuts Restants, Estat Actual, Estat Final, PathFinsAra, PathTotal buit fins el final )
path( 0, Final, Final, Path, Path ).
path( Minutes, CurrentState, FinalState, PathSoFar, TotalPath ) :-
    Minutes > 0, 
    oneStep( MinutesStep, CurrentState, NextState ),
    \+member(NextState, PathSoFar),
    MinutesRemaining is Minutes - MinutesStep,
    path(MinutesRemaining, NextState, FinalState, [NextState|PathSoFar], TotalPath).

% OneStep( Minuts que triga, CurrentState, NextState )
% Una sola persona
oneStep(K, [left, Lleft, Lright], [right, Lleft1, [K|Lright]] ) :- select( K, Lleft, Lleft1 ).
oneStep(K, [right, Lleft, Lright], [left, [K|Lleft], Lright1] ) :- select( K, Lright, Lright1 ).

% Per dos persones
oneStep(K, [left, Lleft, Lright], [right, Lleft2, [K1,K2|Lright]] ) :-  length(Lleft, Len),
                                                                        Len >= 2,
                                                                        select(K1, Lleft, Lleft1),
                                                                        select(K2, Lleft1, Lleft2),
                                                                        K is max(K1, K2).
oneStep(K, [right, Lleft, Lright], [left, [K1,K2|Lleft], Lright2] ) :-  length(Lright, Len),
                                                                        Len >= 2,
                                                                        select(K1, Lright, Lright1),
                                                                        select(K2, Lright1, Lright2),
                                                                        K is max(K1, K2).
