atom_is_char(A) :- atom_chars(A, [_]).

is_char(A) :- atom(A), atom_is_char(A).

is_lowercase_char(A) :- is_char(A), char_code(A, C), C > 96, C < 123.
is_uppercase_char(A) :- is_char(A), char_code(A, C), C > 64, C < 91.
is_numeric(A) :- is_char(A), char_code(A, C), C > 47, C < 58.
is_alpha(A) :- is_lowercase_char(A); is_uppercase_char(A).

atom_is_lowercases(S) :- atom_chars(S, L), all_of(L, char_is_lowercase).
atom_is_uppercases(S) :- atom_chars(S, L), all_of(L, char_is_uppercase).

char_lowercase(A, A) :- is_lowercase_char(A).
char_lowercase(A, L) :- is_uppercase_char(A),
                        char_code(A, C),
                        T is C + 32,
                        char_code(L, T).

char_uppercase(A, A) :- is_uppercase_char(A).
char_uppercase(A, U) :- is_lowercase_char(A),
                        char_code(A, C),
                        T is C - 32,
                        char_code(U, T).

char_numeric(A, N) :- is_numeric(A),
                      char_code(A, C),
                      N is C - 48.

char_rep(L, U, C) :- is_lowercase_char(L), !,
                     char_uppercase(L, U),
                     char_code(U, S), C is S - 64.

char_rep(L, U, C) :- is_uppercase_char(U), !,
                     char_lowercase(U, L),
                     char_code(U, S), C is S - 64.

char_rep(L, U, C) :- integer(C), !,
                     Sl is C + 96, Su is C + 64,
                     char_code(L, Sl), char_code(U, Su).

% Wanna know what's funny? This whole thing just for char_rep ---
% it would have taken less lines if done by brute force. Ayy lmao.

% Back to Pente...

/**
 * Internal Board representation
 *    A  B  C  D  E  F  G  H  J  K  L  M  N  O  P  Q  R  S  T <-- REP
 *    1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19     --
 * 19 ┌──┬──┬──┬──┬──┬──┬──┬──┬──┬──┬──┬──┬──┬──┬──┬──┬──┬──┐      0
 * 18 ├──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┤      1
 * 17 ├──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┤      2
 * 16 ├──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┤      3
 * 15 ├──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┤      4
 * 14 ├──┼──┼──┼──●──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┤      5
 * 13 ├──┼──┼──┼──┼──┼──┼──┼──┼──┼──●──┼──●──┼──┼──┼──┼──┼──┤      6
 * 12 ├──┼──┼──┼──┼──┼──┼──┼──┼──●──○──┼──○──┼──┼──┼──┼──┼──┤      7
 * 11 ├──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┤      8
 * 10 ├──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┤      9
 *  9 ├──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┤     10
 *  8 ├──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┤     11
 *  7 ├──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┤     12
 *  6 ├──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┤     13
 *  5 ├──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┤     14
 *  4 ├──┼──┼──┼──┼──┼──○──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┤     15
 *  3 ├──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┤     16
 *  2 ├──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┼──┤     17
 *  1 └──┴──┴──┴──┴──┴──┴──┴──┴──┴──┴──┴──┴──┴──┴──┴──┴──┴──┘     18
 *  ^                                                              ^
 * REP                                                             |
 *                                                                 |
 * -- 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 <-- INTERNAL
 *
 * The board above has white pieces at
 *   E14 = [5,5];  K12 = [7,10];  L13 = [6,11];  N13 = [6,13].
 *
 * The board above has black pieces at
 *   G4 = [15,5];  L12 = [7,11];  N12 = [7,13].
 *
 * The above are written in form REP=INTERNAL.
 */

/**
 * toprow_index(+C, ?I).
 *   Returns the 1-index I, in the top row, of some alphanumeric atom C.
 */
toprow_index(U, I) :- is_uppercase_char(U), char_rep(_, U, S), S < 9, I is S.
toprow_index(U, I) :- is_uppercase_char(U), char_rep(_, U, S), S > 9, I is S - 1.
toprow_index(L, I) :- is_lowercase_char(L), char_rep(L, _, S), S < 9, I is S.
toprow_index(L, I) :- is_lowercase_char(L), char_rep(L, _, S), S > 9, I is S - 1.
toprow_index(N, I) :- is_numeric(N), char_numeric(N, I).
toprow_index(I, I) :- integer(I).

/**
 * toprow_internal(+C, ?I).
 *   Returns the 0-index I, in the top row, of some alphanumeric atom C.
 */
toprow_internal(C, I) :- toprow_index(C, J), I is J - 1.

/**
 * toprow_rep(+I, ?C).
 *   Returns the character matching the 0-index I in the top row.
 */
toprow_rep(I, C) :- I + 1 < 9, J is I + 1, char_rep(_, C, J).
toprow_rep(I, C) :- I + 1 > 8, J is I + 2, char_rep(_, C, J).

/**
 * rep_internal(+Size, ?Rep, ?Internal).
 *   Matches, for a given board size, an internal [Row, Col]
 *   with a UI [Row, Col]
 */
rep_internal(Size, [RepRow, RepCol], [IntRow, IntCol]) :-
    var(IntRow), var(IntCol),
    toprow_index(RepCol, I),
    IntRow is Size - RepRow,
    IntCol is I - 1.
rep_internal(Size, [RepRow, RepCol], [IntRow, IntCol]) :-
    var(RepRow), var(RepCol),
    I is IntCol + 1,
    toprow_index(RepCol, I),
    RepRow is Size - IntRow.

/**
 * rep_piece_at(+Board, +Row, +Col, ?E).
 *   E is the piece at position Row|Col in the Board.
 *   Row and Col are Rep.
 */
rep_piece_at(Board, Row, Col, E) :- length(Board, Size),
                                    rep_internal(Size, [Row, Col], [R, C]),
                                    write(R), nl, write(C), nl,
                                    matrix_get(Board, R, C, E).
