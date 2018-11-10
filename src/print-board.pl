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
 * write_board_unit(+P, +Size, +Row, +Col).
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
write_board_unit(c, _, 1, 1) :-
    write('\x2514\\x2500\').

% Bottom-right piece ┘
write_board_unit(c, Size, 1, Size) :-
    write('\x2518\').

% Top-left piece ┌
write_board_unit(c, Size, Size, 1) :-
    write('\x250c\\x2500\').

% Top-right piece ┐
write_board_unit(c, Size, Size, Size) :-
    write('\x2510\').

% Bottom piece ┴
write_board_unit(c, Size, 1, Col) :-
    Col > 1, Col < Size,
    write('\x2534\\x2500\').

% Top piece ┬
write_board_unit(c, Size, Size, Col) :-
    Col > 1, Col < Size,
    write('\x252c\\x2500\').

% Left piece ├
write_board_unit(c, Size, Row, 1) :-
    Row > 1, Row < Size,
    write('\x251c\\x2500\').

% Right piece ┤
write_board_unit(c, Size, Row, Size) :-
    Row > 1, Row < Size,
    write('\x2524\').

% Fill piece ┼
write_board_unit(c, Size, Row, Col) :-
    Row > 1, Row < Size, Col > 1, Col < Size,
    write('\x253c\\x2500\').

/**
 * write_board_left(+P, +Row, +Size).
 *   Print the row's number on the left of the board.
 */
% 19 18 ...
write_board_left_alph(w, Row, _) :- Row < 10, format(' ~d ', Row).
write_board_left_alph(w, Row, _) :- Row > 9, format('~d ', Row).
write_board_left_alph(b, R, Size) :- Row is Size + 1 - R, Row < 10, format(' ~d ', Row).
write_board_left_alph(b, R, Size) :- Row is Size + 1 - R, Row > 9, format('~d ', Row).
% ⑲  ⑱ ...
write_board_left_cnum(w, Row, _) :- C is Row + 9311, write(' '), put_code(C), write(' ').
write_board_left_cnum(b, R, Size) :- C is Size + 1 - R + 9311, write(' '), put_code(C), write(' ').

write_board_left(P, Row, Size) :- write_board_left_alph(P, Row, Size).

/**
 * write_board_top_char(+I).
 *   Write the char at index I.
 *   We choose to skip letter 'I'.
 *   Some board authors prefer to skip 'J', others prefer using numbers.
 */
% A  B ...
write_board_top_char_alph(I) :- I < 9, C is I + 64, write(' '), put_code(C).
write_board_top_char_alph(I) :- I > 8, C is I + 65, write(' '), put_code(C).
% Ⓐ Ⓑ ...
write_board_top_char_ccap(I) :- I < 9, C is I + 9397, write(' '), put_code(C).
write_board_top_char_ccap(I) :- I > 8, C is I + 9398, write(' '), put_code(C).
% ⓐ ⓑ ...
write_board_top_char_cmin(I) :- I < 9, C is I + 9423, write(' '), put_code(C).
write_board_top_char_cmin(I) :- I > 8, C is I + 9424, write(' '), put_code(C).

write_board_top_char(I) :- write_board_top_char_alph(I).

/**
 * write_board_top(+P).
 *   Print the top row of the board.
 */
write_board_top(w, Size) :-
    write('  '),
    forloop_increasing(write_board_top_char, 1, Size),
    nl.
write_board_top(b, Size) :-
    write('  '),
    forloop_decreasing(write_board_top_char, 1, Size),
    nl.

/**
 * write_board_bottom(+next, +WhiteCaptures, +BlackCaptures).
 *   Print the players' captures below the board.
 */
write_board_bottom(w, Wc, Bc) :-
    format('      > White: ~d         Black: ~d\n', [Wc, Bc]).
write_board_bottom(b, Wc, Bc) :-
    format('        White: ~d       > Black: ~d\n', [Wc, Bc]).

/**
 * write_board_line(+L, +P, +Size, +Row).
 *   Print a board row (Size elements) on a given Row.
 *   According to player P's perspective.
 */
write_board_line(L, P, Size, Row) :-
    write_board_left(P, Row, Size),
    lb_foreach_increasing(L, write_board_unit, [Size, Row], 1),
    nl.

/**
 * display_game(+Board, +White, +Black, +next).
 *   Print the whole board.
 */
display_game(Board, player(w, Wc), player(b, Bc), w) :-
    matrix_size(Board, Size, Size),
    write_board_top(w, Size),
    lb_foreach_decreasing(Board, write_board_line, [w, Size], Size),
    write_board_bottom(w, Wc, Bc).

display_game(Board, player(w, Wc), player(b, Bc), b) :-
    matrix_size(Board, Size, Size),
    matrix_reverse(Board, Reversed),
    write_board_top(b, Size),
    lb_foreach_decreasing(Reversed, write_board_line, [b, Size], Size),
    write_board_bottom(b, Wc, Bc).
