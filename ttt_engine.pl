:- use_module(minimax).
:- use_module(tictactoe).

% bestMove(+Pos, -NextPos)
% Compute the best next position NextPos from position Pos with minimax.
bestMove(Pos, NextPos) :-
    minimax(Pos, NextPos, Val),
    write('Move has value '), write(Val), write('.'), nl.

% Start the game.
start :-
    nl,
    write('===================='), nl,
	write('= Prolog TicTacToe ='), nl,
	write('===================='), nl, nl,
	write('Rem : x starts the game'), nl,
	playAskMarker.

% Ask the mark for the human player and start the game with it.
playAskMarker :-
	  nl, write('Marker for human player? (x, o, or [w]atch)'), nl,
	  read(HumanPlayer), nl,
	  (
        % If not o, x, or w -> not a valid mark
        HumanPlayer \= o, HumanPlayer \= x, HumanPlayer \= w, !,
	    write('Error: not a valid marker!'), nl,
	    playAskMarker                            % Ask again
	    ;
	    EmptyBoard = [0, 0, 0, 0, 0, 0, 0, 0, 0],
	    show(EmptyBoard), nl,

	    % x always starts so picking x is picking to go first.
	    play([x, play, EmptyBoard], HumanPlayer)
	  ).

% play(+Position, +HumanPlayer)
% Position here, the 3-element list, might more accurately be called the
% "game state", consisting of the current player, the current state of play,
% and the current board configuration.
% If next player to play in position is equal to HumanPlayer -> Human must play.
play([HumanPlayer, play, Board], HumanPlayer) :-
    HumanPlayer \= w, !,
    nl, write('Position to play?'), nl,
    read(Pos), nl,
    (
      humanMove([HumanPlayer, play, Board],
                [NextPlayer, State, NextBoard], Pos), !,
      show(NextBoard),
      (
        State = win, !,                             % If Player wins -> stop
        nl, write('End of game: '), write(HumanPlayer), write(' wins!'), nl, nl
        ;
        State = draw, !,                            % If draw -> stop
        nl, write('End of game: draw!'), nl, nl
        ;
        play([NextPlayer, play, NextBoard], HumanPlayer) % Else -> continue game
      )
      ;
      write('-> Bad move!'), nl,               % If humanMove fails -> bad move
      play([HumanPlayer, play, Board], HumanPlayer)  % Ask again
    ).

% play(+Position, +HumanPlayer)
% If it is not the human who must play -> the computer must play.
% Compute the best move for the computer.
play([Player, play, Board], HumanPlayer) :-
    nl, write('Computer plays:'), nl,
    % Compute the best move.
    bestMove([Player, play, Board], [NextPlayer, State, BestSuccBoard]),
    show(BestSuccBoard),
    (
      State = win, !,                                 % If Player win -> stop
      nl, write('End of game: '), write(Player), write(' wins!'), nl, nl
      ;
      State = draw, !,                                % If draw -> stop
      nl, write('End of game: draw!'), nl, nl
      ;
      % Else -> continue the game
      play([NextPlayer, play, BestSuccBoard], HumanPlayer)
    ).

% humanMove(+Pos, -NextPos, +MarkPos)
humanMove([HumanPlayer, play, Board], [NextPlayer, State, NextBoard], Pos) :-
    nextPlayer(HumanPlayer, NextPlayer),
    set1(Pos, HumanPlayer, Board, NextBoard),
    (
      winPos(HumanPlayer, NextBoard), !, State = win ;
      drawPos(HumanPlayer,NextBoard), !, State = draw ;
      State = play
    ).

% nextPlayer(Player, NextPlayer)
% True if NextPlayer is the next player to play after Player.
nextPlayer(o, x).
nextPlayer(x, o).

% set1(+Pos, +Elem, +List, -ResList).
% Set Elem at position Pos in List => Result in ResList.
% Counting starts at 1. 0 must be at Pos to succeed.
set1(1, E, [X|Ls], [E|Ls]) :- !, X = 0.

set1(P, E, [X|Ls], [X|L2s]) :-
    number(P),
    P1 is P - 1,
    set1(P1, E, Ls, L2s).

% show(+Board)
% Show the board to current output.
show([X1, X2, X3, X4, X5, X6, X7, X8, X9]) :-
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

% show2(+Term)
% Write the term to current output.
% Replace 0 by ' '.
show2(X) :-
    X = 0, !,
    write(' ').

show2(X) :-
    write(X).
