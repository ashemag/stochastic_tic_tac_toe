:- module(expectiminimax, [expectiminimax/3, expectiminimax/4]).
:- use_module(library(random)).

% expectiminimax(+Pos, -BestNextPos, -Val)
% Pos is a position, Val is its expectiminimax value.
% Best move from Pos leads to position BestNextPos.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implement expectiminimax. You do NOT have to modify any of the other
% functions. This goes for every predicate we ask you to modify. You may
% add new predicates as needed, however.
% FOR PART 3:
% Please keep around an expectiminimax/3:
% Add the cutoff argument then add an expectiminimax/3 which delegaes to it
% like so:

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

expectiminimax(Pos, BestNextPos, Val) :-
    expectiminimax(Pos, -1, BestNextPos, Val).

% CASE: CUTOFF IS 0
expectiminimax(Pos, CutOff, _, Val) :-
    CutOff == 0,
    eval(Pos, Val),
    !.

%% CASE OF CHANCE STRAT %%%
%%% Check if we are at chance strategy. If so, populate with val with expected val %%%
expectiminimax(Pos, CutOff, _, Val) :-
    chance_to_move(Pos),
    DecCutOff is CutOff - 1,
    expectedVal(Pos, DecCutOff, Val),
    !.

%%% CASE OF MIN/MAX %%%
%%% Else, explore next level of tree with strategy level and populate val with bestval  %%%
expectiminimax(Pos, CutOff, BestNextPos, Val) :-
    strat_at(Pos, Strat),
    bagof(NextPos, move(Pos, NextPos), NextPosList),
    DecCutOff is CutOff - 1,
    best(NextPosList, DecCutOff, Strat, BestNextPos, Val), !.

%%% CASE: LEAF NODE %%%
%%% Otherwise, we are at min/max strategy. Check if Pos is leaf node and populate val with utility if so %%%
expectiminimax(Pos, _, _, Val) :-
    utility(Pos, Val).

% One choice to move to automatically means it is the best next position.
best([Pos],CutOff, _, Pos, Val) :-
    expectiminimax(Pos, CutOff, _, Val), !.

% To pick between a number of positions, calculate the minimax of the first one,
% find the best of the rest, then pick whichever was best out of the first or
% the one which was best of the rest.
best([Pos1 | PosList], CutOff, Strat, BestPos, BestVal) :-
    expectiminimax(Pos1, CutOff, _, Val1),
    best(PosList, CutOff, Strat, Pos2, Val2),
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implement expectedVal.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% HELPER %%%
%%% compute bestval from expectiminimax * probability of moving to valid move %%%
expectation(Pos, CutOff, Product) :-
    move(Pos, NextPos),
    expectiminimax(NextPos, CutOff, _, BestVal),
    chance_of(Pos, NextPos, P),
    Product is BestVal * P.

%%% For all possible moves, compute sum of products  %%%
expectedVal(Pos, CutOff, Val) :-
    bagof(Product, expectation(Pos, CutOff, Product), Values),
    sum_list(Values, Val).
