########## PART 1 ##########
[expectiminimax],
[expectiminimax_tree].

expectiminimax(node(node(node(leaf(-1),leaf(0),max),node(leaf(4),leaf(0),max), chance(0.6)),node(node(leaf(-2),leaf(2),max),node(leaf(3),leaf(-3),max),chance(0.5)),min), BestPos, Val).

expectiminimax(node(node(leaf(-1),leaf(0),max),node(leaf(4),leaf(0),max),chance(0.6)), BestPos, Val).


%expectiminimax(leaf(3), BestPos, Val).

expectiminimax(node(leaf(-1), leaf(1), max),X,V).

########## PART 2 ##########
test([o, x, play, [0,0,0, 0,o,0, 0,o,o], 3, 3], BestPos, Val).

move([chance(o), chance(o), play, [x,0,0,0,x,o,0,0,o], 3, 3], NextMove).
utility([chance(o), chance(o), win, [x, x, o, o, o, o,0, 0, 0], _, _], Val).

utility([chance(o), chance(o), win, [x,o,x,x,x,o,o,o,o], 3, 3], Val).


utility([chance(o), _, win, Board, Dim, N], 1) :-
    winPos(PlayingAs, Board, Dim, N),
    x == PlayingAs.

utility([chance(o), _, win, Board, Dim, N], -1) :-
    winPos(PlayingAs, Board, Dim, N),
    o== PlayingAs.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CASE: MUST WIN
% must win board for other player :(
move([Player, PlayingAs, State, Board, Dim, N], [NextPlayer, _, win, NextBoard, Dim, N]) :-
    %playerOption(Player),
    nextPlayer(Player, NextPlayer),
    move_aux(PlayingAs, Board, NextBoard),
    winPos(PlayingAs, NextBoard, Dim, N).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CASE: PLAY WHEN YOU DON'T WANT TO SELECT WIN FOR OPPONENT
% the player != playingAs and moves so as not to select a winning position
move([Player, PlayingAs, State, Board, Dim, N], [NextPlayer, _, play, NextBoard, Dim, N]) :-
    playerOption(Player),
    move_aux(PlayingAs, Board, NextBoard),
    \+ winPos(PlayingAs, NextBoard, Dim, N),
    nextPlayer(Player, NextPlayer).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CASE: MUST WIN
% must win board for other player :(
move([Player, PlayingAs, State, Board, Dim, N], [NextPlayer, _, win, NextBoard, Dim, N]) :-
    playerOption(Player),
    move_aux(PlayingAs, Board, NextBoard),
    winPos(PlayingAs, NextBoard, Dim, N),
    nextPlayer(Player, NextPlayer).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CASE: PLAY WHEN YOU DON'T WANT TO SELECT WIN FOR OPPONENT
% the player != playingAs and moves so as not to select a winning position
move([Player, PlayingAs, State, Board, Dim, N], [NextPlayer, _, play, NextBoard, Dim, N]) :-
    playerOption(Player),
    move_aux(PlayingAs, Board, NextBoard),
    \+ winPos(PlayingAs, NextBoard, Dim, N),
    nextPlayer(Player, NextPlayer).


start_cutoff(3, 3, 2).

    write(OppPlayer),
    write('\n'),
    \+ member(OppPlayer, Line),
    countall(Player, Line, Occurences),
    write(Occurences),



%     | o | o
%  -----------
%     | x | o
%  -----------
%   x |   | x



eval([o, _, _, [o,0,o, 0,0,x, x,0,0], 3, 3], Val).

%   o |   | o
%  -----------
%     |   | x
%  -----------
%   x |   |


eval([o, _, _, [0,o,o, 0,x,o, x,0,x], 3, 3], Val).

%     | o | o
%  -----------
%     | x | o
%  -----------
%   x |   | x



 aggregate_all(count, calcNumberAdjacent(Player, Rows), WinCountRows),
    write(WinCountRows),
    write('\n'),
    aggregate_all(count, calcNumberAdjacent(Player, Cols), WinCountCols),
    write(WinCountCols),
    write('\n'),
    aggregate_all(count, calcNumberAdjacent(Player, [DiagDown, DiagUp]), WinCountDiag),
    write(WinCountDiag),
    write('\n'),
    Total is WinCountRows + WinCountCols + WinCountDiag,
    write(Total),
    write('\n'),
    Val is Total / 8




% if x is playing as x or o is playing as o, we want a winning strategy
move([Player, PlayingAs, State, Board, _, _], [NextPlayer, NextPlayingAs, NextState, NextBoard, _, _]) :-
    print(Player),
    print(PlayingAs),
    Player == PlayingAs,
    nextPlayer(Player, NextPlayer),
    move_aux(Player, Board, NextBoard),
    print("Board\n"),
    print(NextBoard).

% if o is playing as x, we want a losing strategy for x
move([Player, PlayingAs, State, Board, _, _], [NextPlayer, NextPlayingAs, NextState, NextBoard, _, _]) :-
    nextPlayer(Player, NextPlayer),
    move_aux(Player, Board, NextBoard),
    print("Board\n"),
    print(NextBoard).

    % find option not in winning pos
move([Player, PlayingAs, State, Board, Dim, N], [NextPlayer, NextPlayingAs, NextState, NextBoard, Dim, N]) :-
    print("testing move that does not win..."),
    print(Player),
    print(PlayingAs),
    % get all winning moves for player and make sure playingAs doesn't move there
    move_aux(Player, Board, HypNextBoard),
    \+ winPos(Player, HypNextBoard, Dim, N),

    move_aux(PlayingAs, Board, NextBoard),
    NextBoard != HypNextBoard,

    NextState = 'play',
    nextPlayer(Player, NextPlayer),
    nextPlayer(PlayingAs, NextPlayingAs),
    !.



% Winning move for player == playingAs
move([Player, PlayingAs, State, Board, Dim, N], [NextPlayer, NextPlayingAs, NextState, NextBoard, Dim, N]) :-
    print("testing winning move..."),
    Player == PlayingAs,
    nextPlayer(Player, NextPlayer),
    nextPlayer(PlayingAs, NextPlayingAs),
    print(Player),
    print(PlayingAs),
    print(NextPlayer),
    move_aux(PlayingAs, Board, NextBoard),
    winPos(PlayingAs, NextBoard, Dim, N),
    print(NextBoard), !.

% characterizes move that leads to a draw
move([Player, PlayingAs, State, Board, Dim, N], [NextPlayer, NextPlayingAs, NextState, NextBoard, Dim, N]) :-
    print("testing draw move..."),
    nextPlayer(Player, NextPlayer),
    nextPlayer(PlayingAs, NextPlayingAs),
    print(Player),
    print(PlayingAs),
    print(NextPlayer),
    move_aux(PlayingAs, Board, NextBoard),
    drawPos(NextBoard, Dim, N),
    print(NextBoard), !.
