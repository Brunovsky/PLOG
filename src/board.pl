/**
 * make_board(Size, Board).
 *   Constructs a SizexSize board with no pieces (all c) to output Board.
 */
make_board(Size, Board) :- fill_n(Size, c, L), fill_n(Size, L, Board).

/**
 * game_over(Board, P).
 *   Verifies if the Board position has a winner (w or b).
 *   Fails if there is no winner.
 */
