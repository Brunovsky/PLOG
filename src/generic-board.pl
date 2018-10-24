/**
 * MEDIUM WHITE CIRCLE:     u+25cf
 * MEDIUM BLACK CIRCLE:     u+25cb
 * 
 * LARGE WHITE CIRCLE:      u+25ef
 * LARGE BLACK CIRCLE:      u+2b24
 *
 * SMALL WHITE CIRCLE:      u+26aa 
 * SMALL BLACK CIRCLE:      u+26ab
 *
 * Circled A-Z:             u+24b6 - u+24cf  (9398)
 * Circled 1-20:            u+2460 - u+2473  (9312)
 * Other circles around these unicode codes.
 *
 * UNICODE BOX DRAWING
 * http://jrgraphix.net/r/Unicode/2500-257F
 * 
 *     Bot left             u+2514
 *     Bot right            u+2518
 *     Top left             u+250c
 *     Top right            u+2510
 *     Bottom               u+2534
 *     Top                  u+252c
 *     Left                 u+251c
 *     Right                u+2524
 *     Fill                 u+253c
 *
 * HORIZONTAL DASH:    9472 u+2500
 * VERTICAL DASH:      9474 u+2502
 */

%print_unicodes(X, N) :- N = 0; M is N-1, K is X+1, put_code(X), put_char(' '), print_unicodes(K, M).

/**
 * white        '\x25cf\'
 * black        '\x25cb\'
 * bot left     '\x2514\'
 * bot right    '\x2518\'
 * top left     '\x250c\'
 * top right    '\x2510\'
 * bottom       '\x2534\'
 * top          '\x252c\'
 * left         '\x251c\'
 * right        '\x2524\'
 * fill         '\x253c\'
 */

/**
 * write_board_each(P, Row, Col, Size).
 *   Writes to the console the piece whose internal representation is P,
 *   on location (Row, Col).
 */
% White piece, anywhere
write_board_each(w, _, _, _) :-
    write('\x25cf\ ').

% Black piece, anywhere
write_board_each(b, _, _, _) :-
    write('\x25cb\ ').

% Bottom-left piece, A1
write_board_each(c, 1, 1, _) :-
    write('\x2514\\x2500\').

% Bottom-right piece, T1
write_board_each(c, Size, 1, Size) :-
    write('\x2518\').

% Top-left piece, A19
write_board_each(c, 1, Size, Size) :-
    write('\x250c\\x2500\').

% Top-right piece, T19
write_board_each(c, Size, Size, Size) :-
    write('\x2510\').

% Bottom piece, B1-S1
write_board_each(c, Col, 1, Size) :-
    Col > 1, Col < Size,
    write('\x2534\\x2500\').

% Top piece, B19-S19
write_board_each(c, Col, Size, Size) :-
    Col > 1, Col < Size,
    write('\x252c\\x2500\').

% Left piece, A2-A18
write_board_each(c, 1, Row, Size) :-
    Row > 1, Row < Size,
    write('\x251c\\x2500\').

% Right piece, T2-T18
write_board_each(c, Size, Row, Size) :-
    Row > 1, Row < Size,
    write('\x2524\').

% Fill piece, B2-S18
write_board_each(c, Col, Row, Size) :-
    Row > 1, Row < Size, Col > 1, Col < Size,
    write('\x253c\\x2500\').

/**
 * write_board_left(next, Row).
 *   Print the row's number on the left of the board.
 */
write_board_left(w, Row, _) :- Row < 10, format(' ~d ', Row).
write_board_left(w, Row, _) :- Row > 9, format('~d ', Row).
write_board_left(b, R, Size) :- Row is Size + 1 - R, Row < 10, format(' ~d ', Row).
write_board_left(b, R, Size) :- Row is Size + 1 - R, Row > 9, format('~d ', Row).

/**
 * write_board_top_char(I).
 *   Write the char at index I.
 *   We choose to skip letter I.
 *   Some board authors prefer to skip J, others prefer using numbers.
 */
write_board_top_char(I) :- I < 9, C is I + 64, put_char(' '), put_code(C).
write_board_top_char(I) :- I > 8, C is I + 65, put_char(' '), put_code(C).

/**
 * write_board_top(next).
 *   Print the top row of the board.
 */
write_board_top(w, Size) :-
    write('  '),
    loop_increasing(write_board_top_char, 1, Size),
    nl.
write_board_top(b, Size) :-
    write('  '),
    loop_decreasing(write_board_top_char, 1, Size),
    nl.

/**
 * write_board_bottom(next, WhiteCaptures, BlackCaptures).
 *   Print the players' captures below the board.
 */
write_board_bottom(w, Wc, Bc) :-
    format('      > White: ~d         Black: ~d\n', [Wc, Bc]).
write_board_bottom(b, Wc, Bc) :-
    format('        White: ~d       > Black: ~d\n', [Wc, Bc]).

/**
 * write_board_line(L, Row, Size, P).
 *   Print a board row (Size elements) on a given Row.
 *   According to player P's perspective.
 */
write_board_line(L, Row, Size, P) :-
    write_board_left(P, Row, Size),
    l_foreach_increasing(L, write_board_each, 1, [Row, Size]), write('\n').

/**
 * display_game(Board)
 *   Print the whole board.
 */
display_game(Board, player(white, Wc), player(black, Bc), w) :-
    check_matrix(Board, Size, Size),
    write_board_top(w, Size),
    l_foreach_decreasing(Board, write_board_line, Size, [Size, w]),
    write_board_bottom(w, Wc, Bc).

display_game(Board, player(white, Wc), player(black, Bc), b) :-
    check_matrix(Board, Size, Size),
    matrix_reverse(Board, Reversed),
    write_board_top(b, Size),
    l_foreach_decreasing(Reversed, write_board_line, Size, [Size, b]),
    write_board_bottom(b, Wc, Bc).

% reconsult('board.pl').
% reconsult('lists.pl').
