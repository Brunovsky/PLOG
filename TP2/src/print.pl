/**
 * max_width_number/2
 * max_width_number(+Numbers, -Width).
 * Width is the maximum number of digits of any number in Numbers.
 */
max_width_number(Numbers, Width) :-
  flatten(Numbers, Flat),
  map(number_codes, Flat, Codes),
  map(length, Codes, Widths),
  max_member(Width, Widths).

/**
 * center_number/2
 * center_number(+Width, +Num).
 * Left and right padding for writing numbers in board cells.
 */
center_number(Width, Num) :-
  number_codes(Num, NumberCodes),
  length(NumberCodes, L),
  Rest is Width - L,
  LeftL is integer((Rest + 1) / 2),
  RightL is integer(Rest / 2),
  fill_n(LeftL, ' ', LeftSpaces),
  fill_n(RightL, ' ', RightSpaces),
  atom_chars(Left, LeftSpaces),
  atom_chars(Right, RightSpaces),
  write(Left), write(Num), write(Right).

/**
 * write_connector/4
 * write_connector(+Left, +Right, +Up, +Down).
 * Unicode box drawing connection horizontal and vertical dashes between
 * cells in the board.
 */
write_connector(1, 1, 1, 1) :- write(' ').
write_connector(1, 1, 1, 0) :- write('\x2577\').
write_connector(1, 1, 0, 1) :- write('\x2575\').
write_connector(1, 1, 0, 0) :- write('\x2502\').
write_connector(1, 0, 1, 1) :- write('\x2576\').
write_connector(1, 0, 1, 0) :- write('\x250c\').
write_connector(1, 0, 0, 1) :- write('\x2514\').
write_connector(1, 0, 0, 0) :- write('\x251c\').
write_connector(0, 1, 1, 1) :- write('\x2574\').
write_connector(0, 1, 1, 0) :- write('\x2510\').
write_connector(0, 1, 0, 1) :- write('\x2518\').
write_connector(0, 1, 0, 0) :- write('\x2524\').
write_connector(0, 0, 1, 1) :- write('\x2500\').
write_connector(0, 0, 1, 0) :- write('\x252c\').
write_connector(0, 0, 0, 1) :- write('\x2534\').
write_connector(0, 0, 0, 0) :- write('\x253c\').

/**
 * write_multiple/2
 * write_multiple(+N, +Char).
 * Write Char N times.
 */
write_multiple(N, Char) :-
  fill_n(N, Char, Bars),
  atom_chars(String, Bars),
  write(String).

/**
 * write_top/2
 * write_top(+Width, +Vertical).
 * Top bar of the board. 
 */
write_top(Width, VerticalRow) :-
  write('\x250c\'), write_multiple(Width, '\x2500\'),
  ( foreach(Vert, VerticalRow),
    param(Width)
  do  write_connector(0, 0, 1, Vert),
      write_multiple(Width, '\x2500\')
  ),
  write('\x2510\'), nl.

/**
 * write_bot/2
 * write_bot(+Width, +Vertical).
 * Bottom bar of the board.
 */
write_bot(Width, VerticalRow) :-
  write('\x2514\'), write_multiple(Width, '\x2500\'),
  ( foreach(Vert, VerticalRow),
    param(Width)
  do  write_connector(0, 0, Vert, 1),
      write_multiple(Width, '\x2500\')
  ),
  write('\x2518\'), nl.

/**
 * write_number_row/3
 * write_number_row(+Width, +BoardRow, +VerticalRow).
 * Write a number line of the board.
 */
write_number_row(Width, BoardRow, VerticalRow) :-
  BoardRow = [Front|Tail],
  write('\x2502\'),
  (Front = 0 -> write_multiple(Width, ' '); center_number(Width, Front)),
  ( foreach(Num, Tail),
    foreach(Vert, VerticalRow),
    param(Width)
  do  (Vert = 0 -> write('\x2502\'); write(' ')),
      (Num = 0 -> write_multiple(Width, ' '); center_number(Width, Num))
  ),
  write('\x2502\'), nl.

/**
 * write_horz_row/4
 * write_horz_row(+Width, +VerticalUp, +VerticalDown, +HorizontalRow).
 * Write a line in between number lines of the board.
 */
write_horizontals(Width, VerticalUp, VerticalDown, HorizontalRow) :-
  HorizontalRow = [FrontHorz|HorizontalTail],
  write_connector(1, FrontHorz, 0, 0),
  (FrontHorz = 0 -> write_multiple(Width, '\x2500\'); write_multiple(Width, ' ')),
  ( foreach(Horz, HorizontalTail),
    foreach(Up, VerticalUp),
    foreach(Down, VerticalDown),
    fromto(FrontHorz, PreviousHorz, Horz, BackHorz),
    param(Width)
  do  write_connector(PreviousHorz, Horz, Up, Down),
      (Horz = 0 -> write_multiple(Width, '\x2500\'); write_multiple(Width, ' '))
  ),
  write_connector(BackHorz, 1, 0, 0), nl.

/**
 * print_board/3
 * print_board(+Board, +Vertical, +Horizontal).
 * Entry point for board drawing.
 */
print_board(Board, Vertical, Horizontal) :-
  max_width_number(Board, Width),
  Board = [FrontBoard|BoardTail],
  Vertical = [FrontVertical|VerticalTail],
  append(Upper, [BackVertical], Vertical),
  write_top(Width, FrontVertical),
  write_number_row(Width, FrontBoard, FrontVertical),
  ( foreach(BoardRow, BoardTail),
    foreach(VerticalDown, VerticalTail),
    foreach(HorizontalRow, Horizontal),
    foreach(VerticalUp, Upper),
    param(Width)
  do  write_horizontals(Width, VerticalUp, VerticalDown, HorizontalRow),
      write_number_row(Width, BoardRow, VerticalDown)
  ),
  write_bot(Width, BackVertical).

print_test(0) :-
  write_number_row(3, [1,2,3,4,5,6,7,8,9], [0,0,1,0,1,1,0,0]),
  write_number_row(2, [4,5,6,4,5,6], [0,1,1,0,0]),
  write_horizontals(2, [0,1,0,0,0,1,0,1]).

print_test(1) :-
  print_board([
    [1, 2, 3, 4, 5, 6, 7],
    [2, 2, 3, 4, 5, 6, 7],
    [3, 2, 3, 14, 5, 0, 7],
    [4, 2, 3, 4, 5, 6, 7],
    [5, 6, 7, 8, 0, 6, 7]
  ], [
    [ 0, 1, 0, 0, 1, 0],
    [ 1, 1, 0, 1, 0, 1],
    [ 0, 1, 0, 1, 1, 0],
    [ 0, 0, 1, 0, 0, 0],
    [ 1, 0, 0, 1, 1, 0]
  ], [
    [0, 0, 0, 0, 1, 0, 0],
    [1, 0, 0, 1, 0, 0, 1],
    [0, 0, 1, 0, 0, 1, 0],
    [0, 1, 1, 1, 1, 0, 1]
  ]).
