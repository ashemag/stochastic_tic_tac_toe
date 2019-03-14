:- module(minimax, [minimax/3]).

% minimax(Pos, BestNextPos, Val)
% Pos is a position, Val is its minimax value.
% Best move from Pos leads to position BestNextPos.
minimax(Pos, BestNextPos, Val) :-
    bagof(NextPos, move(Pos, NextPos), NextPosList),
    strat_at(Pos, Strat),
    best(NextPosList, BestNextPos, Strat, Val), !.

% If here then above failed and Pos has no successors, i.e. is a leaf node.
minimax(Pos, _, Val) :-
    utility(Pos, Val).

% One choice to move to automatically means it is the best next position.
best([Pos], Pos, _, Val) :-
    minimax(Pos, _, Val), !.

% To pick between a number of positions, calculate the minimax of the first one,
% find the best of the rest, then pick whichever was best out of the first or
% the one which was best of the rest.
best([Pos1 | PosList], BestPos, Strat, BestVal) :-
    minimax(Pos1, _, Val1),
    best(PosList, Pos2, Strat, Val2),
    betterOf(Pos1, Val1, Pos2, Val2, Strat, BestPos, BestVal).

% Pos0 better than Pos1.
betterOf(Pos0, Val0, _, Val1, Strat, Pos0, Val0) :-
    Strat = max,    % MAX prefers the greater value.
    Val0 > Val1, !
    ;
    Strat = min,    % MIN prefers the lesser value.
    Val0 < Val1, !.

% Otherwise Pos1 better than Pos0.
betterOf(_, _, Pos1, Val1, _, Pos1, Val1).
