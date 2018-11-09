/**
 * Internal Board representation
 *    A  B  C  D  E  F  G  H  J  K  L  M  N  O  P  Q  R  S  T
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
 *                                                                --
 *
 * -- 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 --
 *
 * The board above has white pieces at 0-index positions
 *   E14 = [5,5];  K12 = [7,10];  L13 = [6,11];  N13 = [6,13].
 *
 * The board above has black pieces at 0-index positions
 *   G4 = [15,5];  L12 = [7,11];  N12 = [7,13].
 */

/**
 * make_board(Size, Board).
 *   Constructs a SizexSize board with no pieces (all c) to output Board.
 */
make_board(Size, Board) :- fill_n(Size, c, L), fill_n(Size, L, Board).

/**
 * toprow_index(C, I).
 *   Returns the 1-index of character C in the top row.
 */
toprow_index(L, I) :- is_lowercase_char(L), char_rep(L, _, S), S < 9, I is S.
toprow_index(L, I) :- is_lowercase_char(L), char_rep(L, _, S), S > 9, I is S + 1.
toprow_index(U, I) :- is_uppercase_char(U), char_rep(_, U, S), S < 9, I is S.
toprow_index(U, I) :- is_uppercase_char(U), char_rep(_, U, S), S > 9, I is S + 1.

/**
 * (1) piece_at(Board, RowN, ColN, E).
 * (2) piece_at(Board, RowN, ColA, E).
 * (3) piece_at(Board, String, E).
 *   Returns piece (c, w, b) at a given position.
 *   (1) takes row and columns _numbers_.
 *      e.g. piece_at(Board,14,5,w).
 *   (2) takes row number and column _alphabetic_. 
 *      e.g. piece_at(Board,14,'E',w)
 *           piece_at(Board,14,e,w).
 *   (3) takes a position string.
 *      e.g. piece_at(Board,'E14',w).
 *           piece_at(Board,'e14',w).
 */
piece_at(Board, RowN, ColN, E) :- integer(ColN), 
                                length(Board, Rows),
                                R is Rows - RowN,
                                C is ColN - 1,
                                matrix_get(Board, R, C, E).
piece_at(Board, RowN, ColA, E) :- \+ integer(ColA),
                                toprow_index(ColA, ColN),
                                piece_at(Board, RowN, ColN, E).
piece_at(Board, String, E).
