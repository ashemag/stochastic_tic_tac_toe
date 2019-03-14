:- use_module(library(random)).
:- use_module(expectiminimax).
:- use_module(stoch_tictactoe).

% DO NOT MODIFY ANYTHING IN THIS FILE.
% The files you should modify are listed in the coursework description.

% Set flags for full printing of expressions:
% set_prolog_flag(answer_write_options,[max_depth(0)]).
% set_prolog_flag(debugger_write_options,[max_depth(0)]).

test(Pos, BestPos, Val) :-
    expectiminimax(Pos, BestPos, Val).

% Cutoff is for part 3. Ignore until you get there.
% bestMove(+Pos, +Cutoff, -NextPos)
% Compute the best next position NextPos from position Pos with expectiminimax.
bestMove(Pos, Cutoff, NextPos) :-
    write('Running expectiminimax...'),
    get_time(T1),
    (
      Cutoff = -1, !,
      expectiminimax(Pos, NextPos, Val)
      ;
      Cutoff >= -1, !,
      expectiminimax(Pos, Cutoff, NextPos, Val)
    ),
    get_time(T2), Elapsed is floor(T2 - T1),
    nl, write('Took '), write(Elapsed), write('s.'), nl,
    write('Move has value '), write(Val), write('.'), nl.

seed_with(Seed) :-
    % Weird bug requires double invocation as well as the call to random.
    set_random(seed(Seed)),
    random(_),
    set_random(seed(Seed)).

% Start the game using default settings.
start :-
    start_cutoff(3, 3, -1).

% Start the game with a seeded RNG.
% start(+Seed).
start(Seed) :-
    seed_with(Seed),
    start.

% Start the game with specific board properties.
start(Dim, N) :-
    start_cutoff(Dim, N, -1).

% Start the game with a seed and specific board size and win run length.
% start(+BoardDim, +InARow, +Seed)
start(Dim, N, Seed) :-
    seed_with(Seed),
    start_cutoff(Dim, N, -1).

% For part 3
% start(+BoardDim, +InARow, +Cutoff, +Seed)
start_cutoff(Dim, N, Cutoff, Seed) :-
    seed_with(Seed),
    start_cutoff(Dim, N, Cutoff).

% Start the game with specific board size and win run length.
% Cutoff is for part 3.
% start(+BoardDim, +InARow)
start_cutoff(Dim, N, Cutoff) :-
    nl,
    write('=============================='), nl,
	write('= Prolog Stochastic TicTacToe ='), nl,
	write('=============================='), nl, nl,
	write('Each turn, the player\'s mark to place will be decided by coin flip.'), nl,
	playAskMarker(Dim, N, Cutoff).

% Ask the mark for the human player and start the game with it.
playAskMarker(Dim, N, Cutoff) :-
	  nl, write('Marker for human player? (x, o, or [w]atch)'), nl,
	  read(HumanPlayer), nl,
	  (
        % If not o, x, or w -> not a valid mark
	    HumanPlayer \= o, HumanPlayer \= x, HumanPlayer \= w, !,
	    write('Error: not a valid marker!'), nl,
	    playAskMarker(Dim, N, Cutoff)                           % Ask again
	    ;
        BoardSize is Dim * Dim,
        replicate(BoardSize, 0, EmptyBoard),
	    show(EmptyBoard, Dim), nl,

	    % x always starts so picking x is picking to go first.
	    play([chance(x), chance(x), play, EmptyBoard, Dim, N], Cutoff, HumanPlayer)
	  ).

% play(+Position, +HumanPlayer)
% Position here, the 6-element list, might more accurately be called the
% "game state", consisting of the current player, the current mark being placed,
% the current state of play, and the current board configuration. The last two
% parts are the board dimension and win run length and are fixed for the
% duration of play.
% If next player to play in position is equal to HumanPlayer -> Human must play.
play([HumanPlayer, PlayingAs, play, Board, Dim, N], Cutoff, HumanPlayer) :-
    HumanPlayer \= w, !,
    nl, write('Position to play?'), nl,
    read(Pos), nl,
    (
      humanMove([HumanPlayer, PlayingAs, play, Board, Dim, N],
                [NextPlayer, NextPlayingAs, State, NextBoard, Dim, N], Pos), !,
      show(NextBoard, Dim),
      (
        % The same as before, except here we must write out that who the
        % player is *playing as* has won.
        State = win, !,                             % If PlayingAs wins -> stop
        nl, write('End of game: '), write(PlayingAs), write(' wins!'), nl, nl
        ;
        State = draw, !,                            % If draw -> stop
        nl, write('End of game: draw!'), nl, nl
        ;
        % Else -> continue game
        play([NextPlayer, NextPlayingAs, play, NextBoard, Dim, N], Cutoff, HumanPlayer)
      )
      ;
      write('-> Bad move!'), nl,               % If humanMove fails -> bad move
      play([HumanPlayer, PlayingAs, play, Board, Dim, N], Cutoff, HumanPlayer)  % Ask again
    ).

% play(+Position, +HumanPlayer)
% Chance to play.
play([chance(NextPlayer), chance(NextPlayer), play, Board, Dim, N], Cutoff, HumanPlayer) :- !,
    nl, write('Coin flip! '), write(NextPlayer), write(' will play '),
    flip(x, o, NextPlayingAs),
    write(NextPlayingAs),
    play([NextPlayer, NextPlayingAs, play, Board, Dim, N], Cutoff, HumanPlayer).

% play(+Position, +HumanPlayer)
% If it is not the human or chance who must play -> the computer must play.
% Compute the best move for the computer.
play([Player, PlayingAs, play, Board, Dim, N], Cutoff, HumanPlayer) :-
    nl, write('Computer plays:'), nl,
    % Compute the best move.
    bestMove([Player, PlayingAs, play, Board, Dim, N], Cutoff,
             [NextPlayer, NextPlayingAs, State, BestSucBoard, Dim, N]),
    show(BestSucBoard, Dim),
    (
      State = win, !,                                % If PlayingAs wins -> stop
      nl, write('End of game: '), write(PlayingAs), write(' wins!'), nl, nl
      ;
      State = draw, !,                               % If draw -> stop
      nl, write('End of game: draw!'), nl, nl
      ;
      % Else -> continue the game
      play([NextPlayer, NextPlayingAs, play, BestSucBoard, Dim, N], Cutoff, HumanPlayer)
    ).

% humanMove(+Pos, -NextPos, +MarkPos)
humanMove([HumanPlayer, PlayingAs, play, Board, Dim, N],
          [NextPlayer, NextPlayer, State, NextBoard, Dim, N], Pos) :-
    nextPlayer(HumanPlayer, NextPlayer),
    % Second argument is HumanPlayer in the normal game.
    set1(Pos, PlayingAs, Board, NextBoard),
    (
      % In the normal game HumanPlayer is passed in here, not PlayingAs.
      winPos(PlayingAs, NextBoard, Dim, N), !, State = win ;
      drawPos(NextBoard, Dim, N), !, State = draw ;
      State = play
    ).

% nextPlayer(Player, NextPlayer)
% True if NextPlayer is the next player to play after Player.
% chance(P) means P will be playing, but only after a coin has been flipped
% to see which marker they have to use. This stage is explicitly modelled
% rather than included as part of the transition predicates to enable linking
% in with expectiminimax.
nextPlayer(o, chance(x)).
nextPlayer(x, chance(o)).
nextPlayer(chance(x), x).
nextPlayer(chance(o), o).

% flip(+X, +Y, -Choice)
% Makes a uniform random choice between X and Y.
flip(X, Y, Choice) :-
    random_between(0, 1, R),
    (
      R = 0, !, Choice = X
      ;
      R = 1, Choice = Y
    ).

% set1(+Pos, +Elem, +List, -ResList).
% Set Elem at position Pos in List => Result in ResList.
% Counting starts at 1. 0 must be at Pos to Suceed.
set1(1, E, [X|Ls], [E|Ls]) :- !, X = 0.

set1(P, E, [X|Ls], [X|L2s]) :-
    number(P),
    P1 is P - 1,
    set1(P1, E, Ls, L2s).

replicate(N,X,Xs) :-
    length(Xs,N),
    maplist(=(X),Xs).

% show(+Board, +Dim)
% Show the board to current output.
% Specific case of 3x3.
show([X1, X2, X3, X4, X5, X6, X7, X8, X9], _) :-
    !,
    write('   '), show2(X1),
    write(' | '), show2(X2),
    write(' | '), show2(X3), nl,
    write('  -----------'), nl,
    write('   '), show2(X4),
    write(' | '), show2(X5),
    write(' | '), show2(X6), nl,
    write('  -----------'), nl,
    write('   '), show2(X7),
    write(' | '), show2(X8),
    write(' | '), show2(X9), nl.

show(Board, Dim) :-
    winpos:rows(Board, Dim, [TopRow|Rows]),
    showRow(TopRow),
    NumHyphens is 4 * Dim - 1,
    replicate(NumHyphens, '-', HyphenList),
    string_chars(Hyphens, HyphenList),
    forall(member(Row, Rows), (nl, write('  '), write(Hyphens), nl, showRow(Row))).

showRow([FstCell|Row]) :-
    write('   '), show2(FstCell),
    forall(member(Cell, Row), (write(' | '), show2(Cell))).

% show2(+Term)
% Write the term to current output.
% Replace 0 by ' '.
show2(X) :-
    X = 0, !,
    write(' ').

show2(X) :-
    write(X).
