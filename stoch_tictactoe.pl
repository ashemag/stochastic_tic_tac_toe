:- module(stoch_tictactoe, [move/2,utility/2,winPos/4,drawPos/3,strat_at/2,
                            chance_to_move/1,chance_of/3, eval/2]).

:- use_module(winpos).
:- use_module(library(clpfd), [transpose/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Explain here what the assumptions being made in tictactoe.pl (which
% won't carry over) are, as explained in the handout.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The cut in move after winPos in tictactoe allows engine to forget other choices.
% If we are in a win position, we don't have to opt for draw. If we are in draw, we don't have to play.
% In stochastic tictactoe, we only want to cut when our player is playingAs itself and can win AND when there is a draw.
% Otherwise, player still need to examine the other choices.

% The utility predicate in tictactoe can assume we know which player is winning the board.
% In stochastic tictactoe, we must compare the player whose turn it was to the player who won the board (playingAs)
% in order to assign utility +1 for x winning or -1 for o winning.


% True if there is a legal (according to rules) move from Pos to NextPos.
% nextPlayer is implemented in the engine.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implement move. Remember to include a case characterising legal moves from
% chance positions. If chance is playing then both the first and second argument
% to the position (aka game state) are the same, i.e.:
%   [chance(P), chance(P), ...].
% The second to last and last arguments to the game state should always remain
% the same, i.e.:
%   move([Player,     PlayingAs,     State,     Board,     Dim, N],
%        [NextPlayer, NextPlayingAs, NextState, NextBoard, Dim, N])
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% move(+Pos, -NextPos)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% HELPERS

playerOption(o).
playerOption(x).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CASES: CHANCE
move([chance(P), chance(P), play, Board, Dim, N], [P, x, play, Board, Dim, N]).
move([chance(P), chance(P), play, Board, Dim, N], [P, o, play, Board, Dim, N]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CASE: WIN
% we want the player to win the board
move([Player, PlayingAs, _, Board, Dim, N], [NextPlayer, _, win, NextBoard, Dim, N]) :-
    playerOption(Player),
    Player = PlayingAs,
    nextPlayer(Player, NextPlayer),
    move_aux(PlayingAs, Board, NextBoard),
    winPos(PlayingAs, NextBoard, Dim, N),
    !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CASE: DRAW
move([Player, PlayingAs, _, Board, Dim, N], [NextPlayer, _, draw, NextBoard, Dim, N]) :-
    playerOption(Player),
    nextPlayer(Player, NextPlayer),
    move_aux(PlayingAs, Board, NextBoard),
    drawPos(NextBoard, Dim, N), !.

% CASE: WIN WHEN YOU'RE NOT THAT PLAYER
move([Player, PlayingAs, _, Board, Dim, N], [NextPlayer, _, win, NextBoard, Dim, N]) :-
    playerOption(Player),
    Player \= PlayingAs,
    nextPlayer(Player, NextPlayer),
    move_aux(PlayingAs, Board, NextBoard),
    winPos(PlayingAs, NextBoard, Dim, N).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CASE: PLAY
move([Player, PlayingAs, _, Board, Dim, N], [NextPlayer, _, play, NextBoard, Dim, N]) :-
    playerOption(Player),
    nextPlayer(Player, NextPlayer),
    move_aux(PlayingAs, Board, NextBoard),
    \+ winPos(PlayingAs, NextBoard, Dim, N).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% move_aux(+Player, +Board, -NextBoard)
% True if NextBoard is Board with an empty case replaced by a player mark.
move_aux(P, [0|Bs], [P|Bs]).

move_aux(P, [B|Bs], [B|B2s]) :-
    move_aux(P, Bs, B2s).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implement strat_at.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% strat_at(+Pos. -Strat)
% same as tictactoe, 2 strategies MAX/MIN
% [Player,     PlayingAs,     State,     Board,     Dim, N]
strat_at([x, _, play, _, _, _], max).
strat_at([o, _, play, _, _, _], min).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implement chance_to_move. chance_to_move is true if in the given position
% the coin is to be flipped i.e. chance is to "move".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% chance_to_move(+Pos)
% If chance is playing then both the first and second argument
% to the position (aka game state) are the same, i.e.:
%   [chance(P), chance(P), ...].
chance_to_move([chance(P), chance(P), _, _, _, _]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implement chance_of.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% chance_of(+Pos, +NextPos, -Prob)
% Probability of moving from Pos to NextPos at a chance node. (still P, 1 - P)
chance_of(Pos, NextPos, 0.5) :-
    chance_to_move(Pos),
    move(Pos, NextPos).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implement utility. Think carefully about what you do and do not know given
% the position.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%can't win the board by playing as another player
utility([_, _, win, Board, Dim, N], -1) :-
    winPos(o, Board, Dim, N).

utility([_, _, win, Board, Dim, N], 1) :-
    winPos(x, Board, Dim, N).

utility([_, _, draw, _, _, _], 0).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Part 3: Describe your evaluation predicates here.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% (Predicate 1)
% For each player, predicate can calculate the number player has
% for each sequence (row, column, diagonal) that it *can still win*. Utility is normalized
% (for x between 0 and 1 and for o between -1 and 0) with the total number of players that can
% be placed on the board (this is a conservative normalization, a stronger one can be Dim-1*Dim-1 for maximum
% number of players that can be placed on the board without winning the game).

%   o |   | o
%  -----------
%     |   | x
%  -----------
%   x |   |

% In this case, predicate 1 would return +.11 for x and -.33 for o.
% We see that this strategy defines more advantageous positions as corner play (which is why o is winning)

% (Predicate 2)
% For each player, predicate can calculate the number of sequences (row, column, diagonal) that
% it is possible for player to *still win*. Utility should be this value normalized
% (for x between 0 and 1 and for o between -1 and 0) with all sequences of board
% (for 3 x 3, 3 rows + 3 cols + 2 diagonals = 8).

%   o |   | o
%  -----------
%     |   | x
%  -----------
%   x |   |

% In this case, predicate 2 will return +.375 for x and -.375 for o. Both players are in an
% equally advantageous position.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Part 3: Compare your evaluation predicates here.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Predicate 1 encourages corner play because of its emphasis on counting. While this is a
% frequently used strategy, it is not necessarily an indicator of a winning position and can be fooled.

%     | o | o
%  -----------
%     | x | o
%  -----------
%   x |   | x

% We assume its os turn to move as o. x wins +.55 and o wins -.22 even though o will win the game.

% We opt for **PREDICATE 2** as it does not overestimate corner/diagonal strategies.
% However, it is worth noting that a limitation of our implementation of this predicate is that it will not
% count for interdependence between players
% (i.e. player x will not receive more weight if it is in 'more' of a winning position because of
% more xs in the sequence).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Part 3: Implement eval, your utility predicate for partially completed games.
% You should implement two of these, but only one should be called eval when
% you run the game. To do this you can name one eval1, the other eval2,
% and write a separate eval which you can change to delegate to one or the other
% during testing.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% EVAL HELPERS
oppositePlayer(o, x).
oppositePlayer(x, o).

count_helper([],_,0).
count_helper([X|T],X,Y):- count_helper(T,X,Z), Y is 1+Z.
count_helper([X1|T],X,Z):- X1\=X,count_helper(T,X,Z).

countall(List,X,C) :-
    sort(List,List1),
    member(X,List1),
    count_helper(List,X,C).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EVAL ONE

% Calculates the number of Player in sequence not blocked by other player
calcNumberValid(Player, Sequences, Freq) :-
    member(Line, Sequences),
    oppositePlayer(Player, OppPlayer),
    member(Player, Line),
    \+ member(OppPlayer, Line),
    countall(Line, Player, Freq), !.

% no occurrences found
calcNumberValid(_, _, 0).

% Calculates sequences player can still win with emphasis on number of player in that sequence
% Normalizing by total number of a player that can be on board without winning a game
evalOne([Player, _, _, Board, Dim, _], Val) :-
    rows(Board, Dim, Rows),
    transpose(Rows, Cols),
    two_diags(Board, Dim, DiagDown, DiagUp),
    calcNumberValid(Player, Rows, RowCount),
    calcNumberValid(Player, Cols, ColCount),
    calcNumberValid(Player, [DiagDown, DiagUp], DiagCount),
    Total is RowCount + ColCount + DiagCount,
    Val is Total / (Dim*Dim).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EVAL TWO

calcNumberCanWin(Player, Sequences) :-
    member(Line, Sequences),
    oppositePlayer(Player, OppPlayer),
    \+ member(OppPlayer, Line).

% Sequences player can still finish
evalTwo([Player, _, _, Board, Dim, _], Val) :-
    rows(Board, Dim, Rows),
    transpose(Rows, Cols),
    two_diags(Board, Dim, DiagDown, DiagUp),
    aggregate_all(count, calcNumberCanWin(Player, Rows), WinCountRows),
    aggregate_all(count, calcNumberCanWin(Player, Cols), WinCountCols),
    aggregate_all(count, calcNumberCanWin(Player, [DiagDown, DiagUp]), WinCountDiag),
    Total is WinCountRows + WinCountCols + WinCountDiag,
    Val is Total / (Dim*2 + 2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

eval([x, _, _, Board, Dim, N], Val) :-
    evalTwo([x, _, _, Board, Dim, N], Val).

eval([o, _, _, Board, Dim, N], Val) :-
    evalTwo([o, _, _, Board, Dim, N], InterMedVal),
    Val is InterMedVal * -1.

eval(_, 0).

% winPos(+Player, +Board, +Dim, +InARow)
% True if Player wins in Board.
% Special case for 3x3 board.
winPos(P, [X1, X2, X3, X4, X5, X6, X7, X8, X9], _, _) :-
    !, (
    equal(X1, X2, X3, P), ! ;    % 1st line
    equal(X4, X5, X6, P), ! ;    % 2nd line
    equal(X7, X8, X9, P), ! ;    % 3rd line
    equal(X1, X4, X7, P), ! ;    % 1st col
    equal(X2, X5, X8, P), ! ;    % 2nd col
    equal(X3, X6, X9, P), ! ;    % 3rd col
    equal(X1, X5, X9, P), ! ;    % 1st diag
    equal(X3, X5, X7, P)).    % 2nd diag

% Special case for 4x4 board.
winPos(P, [X1,X2,X3,X4, X5,X6,X7,X8, X9,X10,X11,X12, X13,X14,X15,X16], _, _) :-
    !, (
    equal(X1, X2, X3, X4, P), ! ;        % 1st line
    equal(X5, X6, X7, X8, P), ! ;        % 2nd line
    equal(X9, X10, X11, X12, P), ! ;     % 3rd line
    equal(X13, X14, X15, X16, P), ! ;    % 4th line
    equal(X1, X5, X9, X13, P), ! ;       % 1st col
    equal(X2, X6, X10, X14, P), ! ;      % 2nd col
    equal(X3, X7, X11, X15, P), ! ;      % 3rd col
    equal(X4, X8, X12, X16, P), ! ;      % 4th col
    equal(X1, X6, X11, X16, P), ! ;      % 1st diag
    equal(X4, X7, X10, X13, P)).         % 2nd diag

winPos(Player, Board, Dim, N) :-
    winPosGen(Player, Board, Dim, N).

% drawPos(+Board, +Dim, +InARow)
% True if the game is a draw.
drawPos(Board, Dim, N) :-
    \+ member(0, Board),
    \+ winPos(x, Board, Dim, N),
    \+ winPos(o, Board, Dim, N).

% equal(+W, +X, +Y, +Z).
% True if W = X = Y = Z.
equal(X, X, X, X).

% As above for 5.
equal(X, X, X, X, X).
