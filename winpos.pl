:- module(winpos, [winPosGen/4, winLine/3, rows/3, two_diags/4, count/3]).

:- use_module(library(clpfd), [transpose/2]).

% Taken from the library list_util.
split_at(N,Xs,Take,Rest) :-
    split_at_(Xs,N,Take,Rest).

split_at_(Rest, 0, [], Rest) :- !. % optimization
split_at_([], N, [], []) :-
    % cannot optimize here because (+, -, -, -) would be wrong,
    % which could possibly be a useful generator.
    N > 0.
split_at_([X|Xs], N, [X|Take], Rest) :-
    N > 0,
    succ(N0, N),
split_at_(Xs, N0, Take, Rest).

% Counts the number of occurences of an element in a list.
count(List, Elem, N) :-
    include(=(Elem), List, AllElem), length(AllElem, N).

% True if player P has N in a row somewhere in Line.
% winLine(+Player, +InARow, +Line)
winLine(P, N, Line) :-
    winLineAux(P, N, Line, 0).

winLineAux(_, N, _, N).
winLineAux(P, N, [P|Line], SoFar) :-
    SuccSoFar is SoFar + 1,
    winLineAux(P, N, Line, SuccSoFar).
winLineAux(P, N, [X|Line], _) :-
    X \= P,
    winLineAux(P, N, Line, 0).

% rows(+Board, +Dimension, -Rows)
% Dim = 0 loops, so added the condition.
% Board length needs to be a multiple of Dim.
rows([], _, []).
rows(Board, Dim, [Row|Rows]) :-
    Dim > 0,
    split_at(Dim, Board, Row, Rest),
    rows(Rest, Dim, Rows).

% For getting the two main diagonals.
% two_diags(+Board, +BoardDimension, -DiagDown, -DiagUp)
two_diags(Board, Dim, DiagDown, DiagUp) :-
    two_diagsAux(Board, Dim, 1, Dim, DiagDown, DiagUp).

% two_diagsAux(+Board, +BoardDimension, +DownIdx, +UpIdx, -DiagDown, -DiagUp)
two_diagsAux(Board, Dim, LastDown, LastUp, [D], [U]) :-
    LastDown is Dim * Dim,
    LastUp is Dim * Dim - Dim + 1,
    nth1(LastDown, Board, D),
    nth1(LastUp, Board, U).

two_diagsAux(Board, Dim, Down, Up, [D|DiagDown], [U|DiagUp]) :-
    NextDown is Down + Dim + 1,
    NextUp is Up + Dim - 1,
    nth1(Down, Board, D),
    nth1(Up, Board, U),
    two_diagsAux(Board, Dim, NextDown, NextUp, DiagDown, DiagUp).

% Improve this implementation for next time.
diags(Rows, Dim, Diags) :-
    diagIndices(Dim, Indices),
    diagsAux(Rows, Indices, Diags).

diagsAux(_, [], []).
diagsAux(Rows, [FirstIndices|RestIndices], [Diag|Diags]) :-
    indexAll(Rows, FirstIndices, Diag),
    diagsAux(Rows, RestIndices, Diags).

indexAll(_, [], []).
indexAll(Rows, [(Row, Col)|Indices], [Val|Vals]) :-
    nth1(Row, Rows, RowVal),
    nth1(Col, RowVal, Val),
    indexAll(Rows, Indices, Vals).

% Calculates the indices for getting the diagonals. 1-based.
% diagIndices(+N, -Indices)
diagIndices(N, Indices) :-
    diagStarts(N, Starts),
    maplist(diagIndMap, Starts, Indices).

diagIndMap((N, M), Indices) :-
    diagIndOne(N, M, N, Indices).

% Outputs the starting indices for each diagonal.
diagStarts(N, Starts) :-
    diagStartsAux(1, 1, N, Starts).

diagStartsAux(N, N, N, [(N, N)]).
diagStartsAux(Row, Col, N, [(Row, Col)|Rest]) :-
    (
      Row = 1, !,
      (
        Col = N,
        SucRow is Row + 1,
        diagStartsAux(SucRow, Col, N, Rest), !
        ;
        SucCol is Col + 1,
        diagStartsAux(Row, SucCol, N, Rest), !
      )
      ;
      SucRow is Row + 1,
      diagStartsAux(SucRow, Col, N, Rest), !
    ).

% diagIndices for one diagonal, starting at (N, M).
% digIndices(+N, +M, +NOrig, -Indices)
diagIndOne(N, NOrig, NOrig, [(N, NOrig)]).
diagIndOne(N, M, NOrig, [(N,M)|Indices]) :-
    M \= NOrig,
    SuccN is N + 1,
    PredM is M - 1,
    diagIndOne(SuccN, PredM, NOrig, Indices).

% Rotating a matrix rightwards has the effect of replacing the slope-up
% diagonals with the slope-down diagonals.
% Not actually used, just for illustration.
rotate_right(Rows, Rotated) :-
   transpose(Rows, Cols),
   maplist(reverse, Cols, Rotated).

% winPosGen(+Player, +Board, +Dimension, +InARow)
% Generic version of winPos for any board dimension and win-run length.
winPosGen(P, Board, Dim, N) :-
    % First check if there are enough occurences for a possible win.
    count(Board, P, Count),
    Count >= N,
    % Now check every possble line on the board for a win.
    rows(Board, Dim, Rows),
    transpose(Rows, Cols), % Used twice so calculate it here.
    (
      % Either P wins in some row,
      member(Row, Rows),
      winLine(P, N, Row), !
      ;
      % or P wins in some column,
      member(Col, Cols),
      winLine(P, N, Col), !
      ;
      (
        % or P wins in one of the simple diagonals,
        Dim = N, !,
        two_diags(Board, Dim, DiagDown, DiagUp),
        (winLine(P, N, DiagDown), ! ; winLine(P, N, DiagUp), !)
        ;
        % or P wins on one of the slope-up diagonals,
        diags(Rows, Dim, Diags),
        member(Diag, Diags),
        winLine(P, N, Diag), !
        ;
        % or P wins on one of the slope-down diagonals.
        maplist(reverse, Cols, Rotated),
        diags(Rotated, Dim, OtherDiags),
        member(OtherDiag, OtherDiags),
        winLine(P, N, OtherDiag), !
      )
    ).
