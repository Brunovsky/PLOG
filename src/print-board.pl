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
 * write_board_unit/[1,4]
 * write_board_unit(+P, +[RowSize,ColSize], +Row, +Col).
 *   Writes to the console the piece whose internal representation is P,
 *   on location (Row, Col), assuming the board is SizexSize.
 */
% White piece, anywhere ●
write_board_unit(w, _, _, _) :-
    write('\x25cf\ ').

% Black piece, anywhere ○
write_board_unit(b, _, _, _) :-
    write('\x25cb\ ').

% Ⓦ White piece, anywhere Ⓦ.
write_board_unit('W', _, _, _) :-
    write('\x24cc\ ').

% Ⓑ Black piece, anywhere Ⓑ.
write_board_unit('B', _, _, _) :-
    write('\x24b7\ ').

% Bottom-left piece └
write_board_unit(_, _, 1, 1) :-
    write('\x2514\\x2500\').

% Bottom-right piece ┘
write_board_unit(_, [_, ColSize], 1, ColSize) :-
    write('\x2518\').

% Top-left piece ┌
write_board_unit(_, [RowSize, _], RowSize, 1) :-
    write('\x250c\\x2500\').

% Top-right piece ┐
write_board_unit(_, [RowSize,ColSize], RowSize, ColSize) :-
    write('\x2510\').

% Bottom piece ┴
write_board_unit(_, [_, ColSize], 1, Col) :-
    Col > 1, Col < ColSize,
    write('\x2534\\x2500\').

% Top piece ┬
write_board_unit(_, [RowSize,ColSize], RowSize, Col) :-
    Col > 1, Col < ColSize,
    write('\x252c\\x2500\').

% Left piece ├
write_board_unit(_, [RowSize, _], Row, 1) :-
    Row > 1, Row < RowSize,
    write('\x251c\\x2500\').

% Right piece ┤
write_board_unit(_, [RowSize,ColSize], Row, ColSize) :-
    Row > 1, Row < RowSize,
    write('\x2524\').

% Fill piece ┼
write_board_unit(_, [RowSize,ColSize], Row, Col) :-
    Row > 1, Row < RowSize, Col > 1, Col < ColSize,
    write('\x253c\\x2500\').

write_board_unit(w) :- write_board_unit(w, _, _, _), !.

write_board_unit(b) :- write_board_unit(b, _, _, _), !.

write_board_unit('W') :- write_board_unit('W', _, _, _), !.

write_board_unit('B') :- write_board_unit('B', _, _, _), !.

write_board_unit(c) :- write_board_unit(c, [3,3], 2, 2), !.

/**
 * write_npieces(+N, +P).
 *   Writes N pieces of player P.
 */
write_npieces(N, P) :-
    repeat_call(write_board_unit(P), N), !.

/**
 * write_board_left(+P, +Row, +RowSize).
 *   Print the row's number on the left of the board.
 *
 * You may choose alph or cnum for a different representation.
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
 *
 * You may choose alph, ccap or cmin for a different representation.
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
write_board_top(w, ColSize) :-
    write('  '),
    forloop_increasing(write_board_top_char, 1, ColSize),
    nl.
write_board_top(b, ColSize) :-
    write('  '),
    forloop_decreasing(write_board_top_char, 1, ColSize),
    nl.

/**
 * write_bottom/[2,3]
 * write_bottom(+next, +WhiteCaptures, +BlackCaptures).
 *   Print the players' captures below the board.
 */
write_bottom(w, [Wc,Bc]) :-
    write('       > White: '), write_npieces(Wc, b), nl,
    write('         Black: '), write_npieces(Bc, w), nl.

write_bottom(b, [Wc,Bc]) :-
    write('         White: '), write_npieces(Wc, b), nl,
    write('       > Black: '), write_npieces(Bc, w), nl.

write_bottom(w, [Wc,Bc], Turn) :-
    write('       > White: '), write_npieces(Wc, b), nl,
    write('         Black: '), write_npieces(Bc, w), nl,
    format(' Turn: ~d', Turn), nl, !.

write_bottom(b, [Wc,Bc], Turn) :-
    write('         White: '), write_npieces(Wc, b), nl,
    write('       > Black: '), write_npieces(Bc, w), nl,
    format(' Turn: ~d', Turn), nl, !.

/**
 * write_board_line(+L, +P, +[RowSize,ColSize], +Row).
 *   Print a board row (ColSize elements) on a given Row.
 *   According to player P's perspective.
 */
write_board_line(L, P, [RowSize,ColSize], Row) :-
    write_board_left(P, Row, RowSize),
    lb_foreach_increasing(L, write_board_unit, [[RowSize,ColSize], Row], 1),
    nl.

/**
 * print_board/[1,2]
 * print_board(+Board).
 * print_board(+Board, +P).
 *   Print the whole board. The second version flips the board for Black.
 */
print_board(Board, w) :-
    matrix_size(Board, RowSize, ColSize),
    write_board_top(w, ColSize),
    lb_foreach_decreasing(Board, write_board_line, [w, [RowSize,ColSize]], RowSize), !.

print_board(Board, b) :-
    matrix_size(Board, RowSize, ColSize),
    matrix_rowcol_reverse(Board, Reversed),
    write_board_top(b, ColSize),
    lb_foreach_decreasing(Reversed, write_board_line, [b, [RowSize,ColSize]], RowSize), !.

print_board(Board) :- print_board(Board, w), !.

/**
 * display_game/[1,3]
 * display_game(+Game).
 *   Print all the game's board information, plus captures and turn on the bottom.
 */
display_game(Game) :-
    Game = game(Board, P, Cap, Turn, Options),
    opt_flip(Options, Flip),
    Flip, !, print_board(Board, P),
    write_bottom(P, Cap, Turn), !.

display_game(Game) :-
    Game = game(Board, P, Cap, Turn, Options),
    opt_flip(Options, Flip),
    \+ Flip, !, print_board(Board),
    write_bottom(P, Cap, Turn), !.

display_game(Board, P, Cap) :-
    print_board(Board),
    write_bottom(P, Cap), !.
