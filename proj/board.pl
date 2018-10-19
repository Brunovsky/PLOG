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
 * Circled A-Z:        9398 u+24b6 - 9423 u+24cf
 * Circled 1-20:       9312 u+2460 - 9331 u+2473
 *
 * HORIZONTAL DASH:    9472 u+2500
 * VERTICAL DASH:      9474 u+2502
 * http://jrgraphix.net/r/Unicode/2500-257F
 *
 * Other circles around these unicode codes.
 */
%print_codes(X, N) :- N = 0; M is N-1, K is X+1, put_code(X), put_char(' '), print_codes(K, M).

/**
 * player(Cor, Captures).
 *   A player of Pente, with a color, a certain number of Captures and a series of plays.
 * Cor: white or black.
 * Captures: An int (between 0 and 10).
 */
player(_, _).

/**
 * game(Board, White, Black, next).
 *   A game of Pente.
 *   > The current Board is represented by a 19x19 matrix, consisting of
 *     characters c for empty slots, w for White's pieces and b for Black's pieces.
 *   > White and Black are players.
 *   > next is w or b, indicating whose turn it is to play.
 */
game(Board, _, _, _) :- display_game(Board).

/**
 * white        '\x25cf\'
 * black        '\x25cb\'
 * bot left     '\x2514\'
 * bot right    '\x2518\'
 * top left     '\x\'
 * top right    '\x2510\'
 * bottom       '\x2534\'
 * top          '\x252c\'
 * left         '\x251c\'
 * right        '\x2524\'
 * fill         '\x253c\'
 */

/**
 * write_each(P, Row, Col).
 *   Writes to the console the piece whose internal representation is P,
 *   on location (Row, Col).
 */
% White piece, anywhere
write_each(w, _, _) :- write('\x25cf\ ').

% Black piece, anywhere
write_each(b, _, _) :- write('\x25cb\ ').

% Bottom-left piece, A1
write_each(c, 1, 1) :- write('\x2514\\x2500\').

% Bottom-right piece, T1
write_each(c, 1, 19) :- write('\x2518\').

% Top-left piece, A19
write_each(c, 19, 1) :- write('\x250c\\x2500\').

% Top-right piece, T19
write_each(c, 19, 19) :- write('\x2510\').

% Bottom piece, B1-S1
write_each(c, 1, Col) :- Col > 1, Col < 19, write('\x2534\\x2500\').

% Top piece, B19-S19
write_each(c, 19, Col) :- Col > 1, Col < 19, write('\x252c\\x2500\').

% Left piece, A2-A18
write_each(c, Row, 1) :- Row > 1, Row < 19, write('\x251c\\x2500\').

% Right piece, T2-T18
write_each(c, Row, 19) :- Row > 1, Row < 19, write('\x2524\').

% Fill piece, B2-S18
write_each(c, Row, Col) :- Row > 1, Row < 19, Col > 1, Col < 19, write('\x253c\\x2500\').

/**
 * write_left(next, Row).
 *   Print the row's number on the left of the board.
 */
write_left(w, Row) :- Row < 10, format(' ~d ', Row).
write_left(w, Row) :- Row > 9, format('~d ', Row).
write_left(b, R) :- Row is 20-R, Row < 10, format(' ~d ', Row).
write_left(b, R) :- Row is 20-R, Row > 9, format('~d ', Row).

/**
 * write_top(next).
 *   Print the top row of the board.
 */
write_top(w) :- write('   A B C D E F G H J K L M N O P Q R S T\n').
write_top(b) :- write('   T S R Q P O N M L K J H G F E D C B A\n').

/**
 * write_bottom(next, WhiteCaptures, BlackCaptures).
 *   Print the players' captures below the board.
 */
write_bottom(w, Wc, Bc) :- format('        > White: ~d        Black: ~d\n', [Wc, Bc]).
write_bottom(b, Wc, Bc) :- format('          White: ~d      > Black: ~d\n', [Wc, Bc]).

/**
 * Print a board row (19 elements) on a given Row.
 */
write_line(L, P, Row) :- length(L, 19), write_left(P, Row),
                      a_foreach_increasing(L, write_each, Row, 1), write('\n').

/**
 * Print the whole board.
 */
display_game(M, player(white, Wc), player(black, Bc), w) :-
    check_matrix(M, 19, 19),
    write_top(w),
    a_foreach_decreasing(M, write_line, w, 19),
    write_bottom(w, Wc, Bc).

display_game(M, player(white, Wc), player(black, Bc), b) :-
    check_matrix(M, 19, 19),
    matrix_reverse(M, R),
    write_top(b),
    a_foreach_decreasing(R, write_line, b, 19),
    write_bottom(b, Wc, Bc).

/**
 * Game from https://www.youtube.com/watch?v=lNFbF1_eZLQ
 *
 * Early game: After 10 moves.
 * Mid game: After 40 moves.
 * Late game: After 
 */
plog(earlygame) :- display_game([
%    A  B  C  D  E  F  G  H  J  K  L  M  N  O  P  Q  R  S  T
    [c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c], % 19
    [c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c], % 18
    [c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c], % 17
    [c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c], % 16
    [c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c], % 15
    [c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c], % 14
    [c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c], % 13
    [c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c], % 12
    [c, c, c, c, c, c, c, c, b, c, b, c, c, c, c, c, c, c, c], % 11
    [c, c, c, c, c, c, c, c, c, w, w, b, w, c, c, c, c, c, c], % 10
    [c, c, c, c, c, c, c, c, c, b, c, w, c, c, c, c, c, c, c], % 9
    [c, c, c, c, c, c, c, c, c, c, w, c, c, c, c, c, c, c, c], % 8
    [c, c, c, c, c, c, c, c, c, b, c, c, c, c, c, c, c, c, c], % 7
    [c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c], % 6
    [c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c], % 5
    [c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c], % 4
    [c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c], % 3
    [c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c], % 2
    [c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c]  % 1
], player(white, 0), player(black, 0), w).

plog(midgame) :- display_game([
%    A  B  C  D  E  F  G  H  J  K  L  M  N  O  P  Q  R  S  T
    [c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c], % 19
    [c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c], % 18
    [c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c], % 17
    [c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c], % 16
    [c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c], % 15
    [c, c, c, c, c, c, c, c, c, b, c, c, c, b, c, c, c, c, c], % 14
    [c, c, c, c, c, c, c, c, b, c, w, c, w, c, c, c, c, c, c], % 13
    [c, c, c, c, c, c, c, c, c, c, c, w, b, c, c, c, c, c, c], % 12
    [c, c, c, c, c, c, c, c, b, b, w, c, w, c, c, c, c, c, c], % 11
    [c, c, c, c, c, c, c, c, c, w, c, b, w, c, c, c, c, c, c], % 10
    [c, c, c, c, c, c, c, c, b, b, c, w, w, b, c, c, c, c, c], % 9
    [c, c, c, c, c, c, c, c, c, c, w, c, b, c, c, c, c, c, c], % 8
    [c, c, c, c, c, c, c, c, c, b, c, c, c, c, c, c, c, c, c], % 7
    [c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c], % 6
    [c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c], % 5
    [c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c], % 4
    [c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c], % 3
    [c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c], % 2
    [c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c]  % 1
], player(white, 4), player(black, 2), b). 

plog(lategame) :- display_game([
%    A  B  C  D  E  F  G  H  J  K  L  M  N  O  P  Q  R  S  T
    [c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c], % 19
    [c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c], % 18
    [c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c], % 17
    [c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c], % 16
    [c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c], % 15
    [c, c, c, c, c, c, c, c, c, b, c, c, c, b, c, c, c, c, c], % 14
    [c, c, c, c, c, c, c, c, b, c, w, c, w, c, c, c, c, c, c], % 13
    [c, c, c, c, c, c, c, c, b, c, c, w, b, c, c, b, c, c, c], % 12
    [c, c, c, c, c, c, c, c, b, b, w, c, w, c, w, c, c, c, c], % 11
    [c, c, c, c, c, c, c, c, w, w, c, b, c, w, c, c, c, c, c], % 10
    [c, c, c, c, c, c, c, c, b, b, c, w, c, c, b, w, c, c, c], % 9
    [c, c, c, c, c, c, c, c, c, c, w, c, c, c, w, c, c, c, c], % 8
    [c, c, c, c, c, c, c, c, c, b, w, c, c, c, b, c, c, c, c], % 7
    [c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c], % 6
    [c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c], % 5
    [c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c], % 4
    [c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c], % 3
    [c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c], % 2
    [c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c]  % 1
], player(white, 8), player(black, 8), w).

% reconsult('board.pl').
% reconsult('lists.pl').
