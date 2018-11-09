/**
 * make_board(Size, Board).
 *   Constructs a SizexSize board with no pieces (all c) to output Board.
 */
make_board(Size, Board) :- fill_n(Size, c, L), fill_n(Size, L, Board).

/**
 * board_size(Board, Size).
 */
board_size(Board, Size) :- matrix_size(Board, Size, Size).

/**
 * five_consecutive(L, P).
 *   Asserts the list has a five-in-a-row for player P.
 *   Both L and P must be given.
 */
five_consecutive(L, P) :- consecutive(L, P, 5).

/**
 * five_any_row(Board, P).
 *   Asserts the Board has a five-in-a-row for player P in some row.
 */
five_any_row(Board, P) :- a_any_of(Board, five_consecutive, P).

/**
 * five_any_col(Board, P).
 *   Asserts the Board has a five-in-a-row for player P in some column.
 */
five_any_col(Board, P) :- matrix_transpose(Board, T),
                          a_any_of(T, five_consecutive, P).

/**
 * five_any_diag(Board, P).
 *   Asserts the Board has a five-in-a-row for player P in some diagonal.
 */
five_any_diag(Board, P) :- matrix_diagonals(Board, Ds),
                           a_any_of(Ds, five_consecutive, P).

/**
 * five_board(Board, P).
 *   Verifies if the Board position has a five-in-a-row for player P.
 */
five_board(Board, P) :- five_any_row(Board, P);
                        five_any_col(Board, P);
                        five_any_diag(Board, P).

/**
 * 
 */