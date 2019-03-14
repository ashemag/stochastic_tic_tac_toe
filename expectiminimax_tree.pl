:- module(expectiminimax_tree, [move/2,utility/2,strat_at/2,
                                chance_to_move/1,chance_of/3]).

:- use_module(expectiminimax).
:- use_module(library(random)).

% Try testing with the tree:
% node(
%       node(
%             node(
%                   leaf(-1,), leaf(0), max
%                 ),
%             node(
%                   leaf(4), leaf(0), max
%                 ),
%             chance(0.6)
%           ),
%       node(
%             node(
%                   leaf(-2), leaf(2), max
%                 ),
%             node(
%                   leaf(3), leaf(-3), max
%                 ),
%             chance(0.5)
%           ),
%       min
%     )
%
% Should return 1.6 (=0.6*0+0.4*4) as the expectiminimax value of the left
% branch (first subtree).
% Command (without the line breaks):

%expectiminimax(node(node(node(leaf(-1),leaf(0),max),node(leaf(4),leaf(0),max), chance(0.6)),node(node(leaf(-2),leaf(2),max),node(leaf(3),leaf(-3),max),chance(0.5)),min), BestPos, Val).
%test_tree(node(node(node(leaf(-1),leaf(0),max),node(leaf(4),leaf(0),max), chance(0.6)),node(node(leaf(-2),leaf(2),max),node(leaf(3),leaf(-3),max),chance(0.5)),min)).

% Generate binary nat trees of depth N.
% Also stores the player who is moving.
% P is the probability of going left, in chance(P).
gen_tree(0, leaf(V)) :- random_between(-9, 9, V).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implement the node cases for gen_tree.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% MIN %%%
gen_tree(N, node(TL, TR, min)) :-
    0 is N mod 3,
    !,
    succ(M, N),  % As well as getting us M, prevents overlap between 0 case.
    % call on left tree w probability P
    % that has a chance layer of this probability if its called with this
    gen_tree(M, TL),
    gen_tree(M, TR).

%%% MAX %%%
gen_tree(N, node(TL, TR, max)) :-
    2 is N mod 3,
    !, % cut
    succ(M, N),
    gen_tree(M, TL),
    gen_tree(M, TR).

%%% CHANCE %%%
gen_tree(N, node(TL, TR, chance(P))) :-
    succ(M, N),  % As well as getting us M, prevents overlap between 0 case. Reduces layer by 1 M = N - 1
    random(0.0, 1.0, P),
    gen_tree(M, TL),
    gen_tree(M, TR).

strat_at(node(_, _, min), min).
strat_at(node(_, _, max), max).

chance_to_move(node(_, _, chance(_))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implement chance_of/3 here.
%just picking up probability of what was generated, to inform next minimax

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% position is node or leaf position
% to pick up on probability for when we generated gen tree
% ASSUME: we will never call chance_of with two chance nodes
chance_of(node(TL, TR, chance(P)), TR, X) :- X is 1 - P.
chance_of(node(TL, TR, chance(P)), TL, X) :- X is P.


% Choosing either the left or right branch are legal moves.
% There is no legal move from a leaf.
move(node(TL, _, _), TL).
move(node(_, TR, _), TR).

% The leaf stores its utility.
utility(leaf(L), L).

% Generates and solves (in the sense of finding the best next position)
% a tree of depth N.
% solved_tree(+N, -Tree, -BestNext, -Val)
% You can uncomment this when you have implemented expectiminimax.
solved_tree(N, T, Best, Val) :- gen_tree(N, T), expectiminimax(T, Best, Val).
