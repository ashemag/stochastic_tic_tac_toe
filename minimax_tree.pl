:- module(minimax_tree, [move/2,strat_at/2,utility/2]).

:- use_module(minimax).
:- use_module(library(random)).

% Generate binary nat trees of depth N.
% Also stores the player who is moving.
gen_tree(0, leaf(V)) :- random_between(-9, 9, V).
% At even-numbered depths MIN is to move.
gen_tree(N, node(TL, TR, min)) :-
    divmod(N, 2, _, 0),
    !,
    succ(M, N),  % As well as getting us M, prevents overlap between 0 case.
    gen_tree(M, TL),
    gen_tree(M, TR).
% Otherwise (at odd-numbered depths) MAX is to move.
gen_tree(N, node(TL, TR, max)) :-
    succ(M, N),
    gen_tree(M, TL),
    gen_tree(M, TR).

strat_at(node(_, _, min), min).
strat_at(node(_, _, max), max).

% Choosing either the left or right branch are legal moves.
% There is no legal move from a leaf.
move(node(TL, _, _), TL).
move(node(_, TR, _), TR).

% The leaf stores its utility.
utility(leaf(L), L).

% Generates and solves (in the sense of finding the best next position)
% a tree of depth N.
% solved_tree(+N, -Tree, -BestNext, -Val)
solved_tree(N, T, Best, Val) :- gen_tree(N, T), minimax(T, Best, Val).
