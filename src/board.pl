/**
 * make_board(+Size, ?Board).
 *   Constructs a SizexSize board with no pieces (all c) to output Board.
 */
make_board(Size, Board) :- Size > 0, fill_n(Size, c, L), fill_n(Size, L, Board).

/**
 * board_size(+Board, ?Size).
 */
board_size(Board, Size) :- matrix_size(Board, Size, Size).

/**
 * five_board(+Board, +P).
 *   Verifies if the Board position has a five-in-a-row for player P.
 */
five_board(Board, P) :- consecutive_matrix(Board, P, 5).

/**
 * game_over(+game(Board, White, Black, next), ?P).
 *   Verifies if the game is over with winner P (w or b).
 */
game_over(game(Board, player(w, Wc), player(b, Wc), next), w) :-
    five_board(Board, w); Wc =:= 10.
game_over(game(Board, player(w, Wc), player(b, Wc), next), b) :-
    five_board(Board, b); Bc =:= 10.

/**
 * place_stone(+P, +[Row, Col], +Board, ?NewBoard, ?Captures).
 *   Places a stone P (w or b) on Board at position [Row, Col].
 *   Doing so removes captured stones, with the total number of captures
 *   going to Captures.
 */
place_stone(w, [Row, Col], Board, NewBoard, Captures) :-
    matrix_get(Board, Row, Col, c),
    matrix_set()

/**
 * move(+Move, +game(Board, White, Black, next), ?game(Board, White, Black, next)).
 */
