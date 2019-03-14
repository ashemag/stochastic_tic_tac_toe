:- module(tictactoe, [move/2,strat_at/2,utility/2,winPos/2,drawPos/2]).

% move(+Pos, -NextPos)
% True if there is a legal (according to rules) move from Pos to NextPos.
% Pos = [Player, State, Board]
% nextPlayer is implemented in the engine.
move([Player, play, Board], [NextPlayer, win, NextBoard]) :-
    nextPlayer(Player, NextPlayer),
    move_aux(Player, Board, NextBoard),
    winPos(Player, NextBoard), !.

move([Player, play, Board], [NextPlayer, draw, NextBoard]) :-
    nextPlayer(Player, NextPlayer),
    move_aux(Player, Board, NextBoard),
    drawPos(Player,NextBoard), !.

move([Player, play, Board], [NextPlayer, play, NextBoard]) :-
    nextPlayer(Player, NextPlayer),
    move_aux(Player, Board, NextBoard).

% move_aux(+Player, +Board, -NextBoard)
% True if NextBoard is Board with an empty case replaced by a player mark.
move_aux(P, [0|Bs], [P|Bs]).

move_aux(P, [B|Bs], [B|B2s]) :-
    move_aux(P, Bs, B2s).

% strat_at(+Pos. -Strat)
strat_at([x, _, _], max).
strat_at([o, _, _], min).

% utility(+Pos, -Val)
% utility/2 only evaluates the final position.
% We will use  1 when MAX (x) wins
%             -1 when MIN (o) win
%              0 otherwise.
utility([o, win, _], 1).
utility([x, win, _], -1).
utility([_, draw, _], 0).

% winPos(+Player, +Board)
% True if Player wins in Board.
winPos(P, [X1, X2, X3, X4, X5, X6, X7, X8, X9]) :-
    equal(X1, X2, X3, P), ! ;    % 1st line
    equal(X4, X5, X6, P), ! ;    % 2nd line
    equal(X7, X8, X9, P), ! ;    % 3rd line
    equal(X1, X4, X7, P), ! ;    % 1st col
    equal(X2, X5, X8, P), ! ;    % 2nd col
    equal(X3, X6, X9, P), ! ;    % 3rd col
    equal(X1, X5, X9, P), ! ;    % 1st diag
    equal(X3, X5, X7, P).        % 2nd diag

% drawPos(+Player, +Board)
% True if the game is a draw.
drawPos(_,Board) :-
    \+ member(0, Board).

% equal(+W, +X, +Y, +Z).
% True if W = X = Y = Z.
equal(X, X, X, X).
