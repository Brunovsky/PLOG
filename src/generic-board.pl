/**
 * UNICODE BOX DRAWING
 * http://jrgraphix.net/r/Unicode/2500-257F
 *
 * ● White circle      u+25cf     '\x25cf\'
 * ○ Black circle      u+25cb     '\x25cb\'
 *
 * Large white         u+25ef     '\x25ef\'
 * Large black         u+2b24     '\x2b24\'
 *
 * Small white         u+26aa     '\x26aa\'
 * Small black         u+26ab     '\x26ab\'
 * 
 * ┌ Top left          u+250c     '\x250c\'
 * ┐ Top right         u+2510     '\x2510\'
 * └ Bot left          u+2514     '\x2514\'
 * ┘ Bot right         u+2518     '\x2518\'
 *     
 * ├ Left              u+251c     '\x251c\'
 * ┬ Top               u+252c     '\x252c\'
 * ┤ Right             u+2524     '\x2524\'
 * ┴ Bottom            u+2534     '\x2534\'
 *     
 * ┼ Fill              u+253c     '\x253c\'
 *
 * Circled a-z         u+24d0 -- u+24e9
 *  ⓐⓑⓒⓓⓔⓕⓖⓗⓘⓙⓚⓛⓜⓝⓞⓟⓠⓡⓢⓣⓤⓥⓦⓧⓨⓩ
 * Circled A-Z         u+24b6 -- u+24cf
 *  ⒶⒷⒸⒹⒺⒻⒼⒽⒾⒿⓀⓁⓂⓃⓄⓅⓆⓇⓈⓉⓊⓋⓌⓍⓎⓏ
 * Circled 1-20        u+2460 -- u+2473
 *  ①②③④⑤⑥⑦⑧⑨⑩⑪⑫⑬⑭⑮⑯⑰⑱⑲⑳
 *
 * ─ Horizontal dash   u+2500
 * │ Vertical dash     u+2502
 */

/**
 * Internal indexing in the board
 *    1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 ...
 * 19 ┌──┬──┬──┬──┬──┬──┬──┬──┬──┬──┬──┬──┬──┬──┬──┬──┬──┬──┐
 * 18 ├──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┤
 * 17 ├──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┤
 * 16 ├──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┤
 * 15 ├──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┤
 * 14 ├──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┤
 * 13 ├──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┤
 * 12 ├──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┤
 * 11 ├──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┤
 * 10 ├──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┤  ...
 *  9 ├──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┤
 *  8 ├──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┤
 *  7 ├──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┤
 *  6 ├──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┤
 *  5 ├──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┤
 *  4 ├──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┤
 *  3 ├──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┤
 *  2 ├──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┤
 *  1 └──┴──┴──┴──┴──┴──┴──┴──┴──┴──┴──┴──┴──┴──┴──┴──┴──┴──┘
 * ...                          ...                            ...
 */

/**
 * write_board_unit(P, Row, Col, Size).
 *   Writes to the console the piece whose internal representation is P,
 *   on location (Row, Col), assuming the board is SizexSize.
 */
% White piece, anywhere ●
write_board_unit(w, _, _, _) :-
    write('\x25cf\ ').

% Black piece, anywhere ○
write_board_unit(b, _, _, _) :-
    write('\x25cb\ ').

% Bottom-left piece └
write_board_unit(c, 1, 1, _) :-
    write('\x2514\\x2500\').

% Bottom-right piece ┘
write_board_unit(c, Size, 1, Size) :-
    write('\x2518\').

% Top-left piece ┌
write_board_unit(c, 1, Size, Size) :-
    write('\x250c\\x2500\').

% Top-right piece ┐
write_board_unit(c, Size, Size, Size) :-
    write('\x2510\').

% Bottom piece ┴
write_board_unit(c, Col, 1, Size) :-
    Col > 1, Col < Size,
    write('\x2534\\x2500\').

% Top piece ┬
write_board_unit(c, Col, Size, Size) :-
    Col > 1, Col < Size,
    write('\x252c\\x2500\').

% Left piece ├
write_board_unit(c, 1, Row, Size) :-
    Row > 1, Row < Size,
    write('\x251c\\x2500\').

% Right piece ┤
write_board_unit(c, Size, Row, Size) :-
    Row > 1, Row < Size,
    write('\x2524\').

% Fill piece ┼
write_board_unit(c, Col, Row, Size) :-
    Row > 1, Row < Size, Col > 1, Col < Size,
    write('\x253c\\x2500\').

/**
 * write_board_left(next, Row, Size).
 *   Print the row's number on the left of the board.
 */
write_board_left(w, Row, _) :- Row < 10, format(' ~d ', Row).
write_board_left(w, Row, _) :- Row > 9, format('~d ', Row).
write_board_left(b, R, Size) :- Row is Size + 1 - R, Row < 10, format(' ~d ', Row).
write_board_left(b, R, Size) :- Row is Size + 1 - R, Row > 9, format('~d ', Row).

/**
 * write_board_top_char(I).
 *   Write the char at index I.
 *   We choose to skip letter 'I'.
 *   Some board authors prefer to skip 'J', others prefer using numbers.
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
    l_foreach_increasing(L, write_board_unit, 1, [Row, Size]), write('\n').

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
