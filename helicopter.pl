
%% This code solves the following problem:
%% 
%% We have a helicopter that can carry its pilot plus at most two
%% passengers at the same time.  We are given a list of passengers, each
%% with his/her origin and destination, given as coordinates in the plane
%% (a pair of integers in 0..1000).  The aim is to find the shortest route
%% for the helicopter to start from its base (coordinates), handle all
%% passengers, and return to its base.
%% 
%% For this exam: modify this code to allow pilot + three passengers (instead of two).

%% Example input with 5 passengers:
helicopterBasePoint([254,372]).
example(5, [ [[813,136],[133,888]], [[942,399],[600,89]], [[532,241],[55,55]], [[498,276],[553,528]], [[531,911],[430,545]] ]).
%% Allowing pilot + two   passengers, the optimal is      3981.30 km.
%% Allowing pilot + three passengers, the optimal becomes 3899.86 km.




distanceBetweenTwoPoints( [X1,Y1], [X2,Y2],  D ):- D is sqrt( abs(X1-X2)**2 + abs(Y1-Y2)**2 ).  % Pythagoras!

storeRouteIfBetter( Km, Route ):-  bestRouteSoFar( BestKm, _ ), Km < BestKm,
    write('Improved solution. New best distance is '), write(Km), write(' km.'),nl,
    retractall(bestRouteSoFar(_,_)), assertz(bestRouteSoFar(Km,Route)),!.

main:- N=5, retractall(bestRouteSoFar(_,_)),  assertz(bestRouteSoFar(100000,[])),  % "infinite" distance
       example( N, Passengers ),
       helicopterBasePoint( CurrentPoint ),
       heli( Passengers, 0, [CurrentPoint], [] ).
main:- bestRouteSoFar(Km,ReverseRoute), reverse( ReverseRoute, Route ), nl,
       write('Optimal route: '), write(Route), write('. '), write(Km), write(' km.'), nl, nl, halt.



% heli( Passengers, AccumulatedDistance, RouteSoFar, DestinationsOfPassengersInHelicopter )

% Backtrack if AccumulatedDistance is already larger than the distance of the best solution so far:
heli( _, AccumulatedDistance, _, _):-  bestRouteSoFar(Distance,_),  AccumulatedDistance >= Distance, !, fail.

% No passengers to pick up, no passengers left in helicopter -->  return to base
heli( [], AccumulatedDistance, [CurrentPoint|RouteSoFar], [] ):-
    helicopterBasePoint( P ),
    distanceBetweenTwoPoints( CurrentPoint, P, Dist ), NewBest is AccumulatedDistance+Dist,
    storeRouteIfBetter( NewBest, [P,CurrentPoint|RouteSoFar] ), fail.

% Pick up another passenger:
heli( Passengers, AccumulatedDistance, [CurrentPoint|RouteSoFar], DestinationsOfPassengersInHelicopter ):-
    DestinationsOfPassengersInHelicopter \= [_,_,_|_], % Poden haver-hi fins a 3 passatgers instanciats
    select( [Orig,Dest], Passengers, Passengers1 ),  % select a passenger to pick up
    distanceBetweenTwoPoints( CurrentPoint, Orig, D),     AccumulatedDistance1 is AccumulatedDistance+D,
    heli( Passengers1, AccumulatedDistance1, [Orig,CurrentPoint|RouteSoFar], [Dest|DestinationsOfPassengersInHelicopter] ).

% Drop off one of the passengers in the helicopter:
heli( Passengers, AccumulatedDistance, [CurrentPoint|RouteSoFar], DestinationsOfPassengersInHelicopter ):-
    select( Dest, DestinationsOfPassengersInHelicopter, DestinationsOfPassengersInHelicopter1 ),
    distanceBetweenTwoPoints( CurrentPoint, Dest, D),     AccumulatedDistance1 is AccumulatedDistance+D,
    heli( Passengers, AccumulatedDistance1, [Dest,CurrentPoint|RouteSoFar], DestinationsOfPassengersInHelicopter1 ).
