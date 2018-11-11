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
game_over(game(Board, Wc, _Bc, _Next), w) :-
	  five_board(Board, w);
	 	Wc >= 10.
game_over(game(Board, _Wc, Bc, _Next), b) :-
	  five_board(Board, b); 
		Bc >= 10.

/**
 * check_dead_stones_left(+P, +L).
 *   Remember: suicides are not allowed! That's what the caps
 *   trick is for, and why we have to check left and right.
 */
check_dead_stones_left(w, L) :- sublist([w,b,b,'W'], L).
check_dead_stones_left(b, L) :- sublist([b,w,w,'B'], L).

/**
 * check_dead_stones_right(+P, +L).
 *   Remember: suicides are not allowed! That's what the caps
 *   trick is for, and why we have to check left and right.
 */
check_dead_stones_right(w, L) :- sublist(['W',b,b,w], L).
check_dead_stones_right(b, L) :- sublist(['B',w,w,b], L).

/**
 * remove_dead_stones_rowleft(+P, +Board, +[Row, Col], ?NewBoard, ?Captures).
 *   Remove dead stones on the row, left of (Row,Col), killed by P.
 */
remove_dead_stones_rowleft(P, Board, [Row, _], Board, 0) :-
    matrix_row(Board, Row, List),
    \+ check_dead_stones_left(P, List).

remove_dead_stones_rowleft(P, Board, [Row, Col], NewBoard, 2) :-
    matrix_row(Board, Row, List),
    check_dead_stones_left(P, List),
    C1 is Col - 1, C2 is Col - 2,
    matrix_set(Board, Row, C1, c, Inter),
    matrix_set(Inter, Row, C2, c, NewBoard).

/**
 * remove_dead_stones_rowright(+P, +Board, +[Row, Col], ?NewBoard, ?Captures).
 *   Remove dead stones on the row, right of (Row,Col), killed by P.
 */
remove_dead_stones_rowright(P, Board, [Row, _], Board, 0) :-
    matrix_row(Board, Row, List),
    \+ check_dead_stones_right(P, List).

remove_dead_stones_rowright(P, Board, [Row, Col], NewBoard, 2) :-
    matrix_row(Board, Row, List),
    check_dead_stones_right(P, List),
    C1 is Col + 1, C2 is Col + 2,
    matrix_set(Board, Row, C1, c, Inter),
    matrix_set(Inter, Row, C2, c, NewBoard).

/**
 * remove_dead_stones_colabove(+P, +Board, +[Row, Col], ?NewBoard, ?Captures).
 *   Remove dead stones on the column, above of (Row,Col), killed by P.
 */
remove_dead_stones_colabove(P, Board, [_, Col], Board, 0) :-
    matrix_col(Board, Col, List),
    \+ check_dead_stones_left(P, List).

remove_dead_stones_colabove(P, Board, [Row, Col], NewBoard, 2) :-
    matrix_col(Board, Col, List),
    check_dead_stones_left(P, List),
    R1 is Row - 1, R2 is Row - 2,
    matrix_set(Board, R1, Col, c, Inter),
    matrix_set(Inter, R2, Col, c, NewBoard).

/**
 * remove_dead_stones_colbelow(+P, +Board, +[Row, Col], ?NewBoard, ?Captures).
 *   Remove dead stones on the column, below of (Row,Col), killed by P.
 */
remove_dead_stones_colbelow(P, Board, [_, Col], Board, 0) :-
    matrix_col(Board, Col, List),
    \+ check_dead_stones_right(P, List).

remove_dead_stones_colbelow(P, Board, [Row, Col], NewBoard, 2) :-
    matrix_col(Board, Col, List),
    check_dead_stones_right(P, List),
    R1 is Row + 1, R2 is Row + 2,
    matrix_set(Board, R1, Col, c, Inter),
    matrix_set(Inter, R2, Col, c, NewBoard).

/**
 * remove_dead_stones_leftleft(+P, +Board, +[Row, Col], ?NewBoard, ?Captures).
 *   Remove dead stones on the left diagonal, left of (Row,Col), killed by P.
 */
remove_dead_stones_leftleft(P, Board, [Row, Col], Board, 0) :-
    matrix_left_diag_through(Board, Row, Col, List),
    \+ check_dead_stones_left(P, List).

remove_dead_stones_leftleft(P, Board, [Row, Col], NewBoard, 2) :-
    matrix_left_diag_through(Board, Row, Col, List),
    check_dead_stones_left(P, List),
    R1 is Row - 1, R2 is Row - 2,
    C1 is Col - 1, C2 is Col - 2,
    matrix_set(Board, R1, C1, c, Inter),
    matrix_set(Inter, R2, C2, c, NewBoard).

/**
 * remove_dead_stones_leftright(+P, +Board, +[Row, Col], ?NewBoard, ?Captures).
 *   Remove dead stones on the left diagonal, right of (Row,Col), killed by P.
 */
remove_dead_stones_leftright(P, Board, [Row, Col], Board, 0) :-
    matrix_left_diag_through(Board, Row, Col, List),
    \+ check_dead_stones_right(P, List).

remove_dead_stones_leftright(P, Board, [Row, Col], NewBoard, 2) :-
    matrix_left_diag_through(Board, Row, Col, List),
    check_dead_stones_right(P, List),
    R1 is Row + 1, R2 is Row + 2,
    C1 is Col + 1, C2 is Col + 2,
    matrix_set(Board, R1, C1, c, Inter),
    matrix_set(Inter, R2, C2, c, NewBoard).

/**
 * remove_dead_stones_rightleft(+P, +Board, +[Row, Col], ?NewBoard, ?Captures).
 *   Remove dead stones on the right diagonal, left of (Row,Col), killed by P.
 */
remove_dead_stones_rightleft(P, Board, [Row, Col], Board, 0) :-
    matrix_right_diag_through(Board, Row, Col, List),
    \+ check_dead_stones_left(P, List).

remove_dead_stones_rightleft(P, Board, [Row, Col], NewBoard, 2) :-
    matrix_right_diag_through(Board, Row, Col, List),
    check_dead_stones_left(P, List),
    R1 is Row - 1, R2 is Row - 2,
    C1 is Col + 1, C2 is Col + 2,
    matrix_set(Board, R1, C1, c, Inter),
    matrix_set(Inter, R2, C2, c, NewBoard).

/**
 * remove_dead_stones_rightright(+P, +Board, +[Row, Col], ?NewBoard, ?Captures).
 *   Remove dead stones on the right diagonal, right of (Row,Col), killed by P.
 */
remove_dead_stones_rightright(P, Board, [Row, Col], Board, 0) :-
    matrix_right_diag_through(Board, Row, Col, List),
    \+ check_dead_stones_right(P, List).

remove_dead_stones_rightright(P, Board, [Row, Col], NewBoard, 2) :-
    matrix_right_diag_through(Board, Row, Col, List),
    check_dead_stones_right(P, List),
    R1 is Row + 1, R2 is Row + 2,
    C1 is Col - 1, C2 is Col - 2,
    matrix_set(Board, R1, C1, c, Inter),
    matrix_set(Inter, R2, C2, c, NewBoard).

/**
 * remove_dead_stones(+P, +Board, +[Row, Col], ?NewBoard, ?Captures).
 *   Remove all dead stones (rows, columns and diagonals).
 *   Remember: suicides are not allowed!
 */
remove_dead_stones(P, Board, [Row, Col], NewBoard, Captures) :-
    remove_dead_stones_rowleft(P, Board, [Row, Col], New1, C1),
    remove_dead_stones_rowright(P, New1, [Row, Col], New2, C2),
    remove_dead_stones_colabove(P, New2, [Row, Col], New3, C3),
    remove_dead_stones_colbelow(P, New3, [Row, Col], New4, C4),
    remove_dead_stones_leftleft(P, New4, [Row, Col], New5, C5),
    remove_dead_stones_leftright(P, New5, [Row, Col], New6, C6),
    remove_dead_stones_rightleft(P, New6, [Row, Col], New7, C7),
    remove_dead_stones_rightright(P, New7, [Row, Col], NewBoard, C8),
    Captures is C1 + C2 + C3 + C4 + C5 + C6 + C7 + C8.

/**
 * place_stone(+P, +Board, +[Row, Col], ?NewBoard, ?Captures).
 *   Places a stone P (w or b) on Board at position [Row, Col].
 *   Doing so removes captured stones from the board, binding the
 *   number of removed stones to Captures.
 */
place_stone(w, Board, [Row, Col], NewBoard, Captures) :-
		matrix_set(Board, Row, Col, 'W', WBoard),
    remove_dead_stones(w, WBoard, [Row, Col], Removed, Captures),
    matrix_set(Removed, Row, Col, w, NewBoard).

place_stone(b, Board, [Row, Col], NewBoard, Captures) :-
    matrix_set(Board, Row, Col, 'B', BBoard),
    remove_dead_stones(b, BBoard, [Row, Col], Removed, Captures),
    matrix_set(Removed, Row, Col, b, NewBoard).

/**
 * empty_position(+Board, ?[Row, Col]).
 *   Asserts (Row,Col) is an empty position (c) on the Board.
 *   Provides all such positions.
 */
empty_position(Board, [Row, Col]) :- matrix_indices(Board, c, [Row, Col]).

/**
 * empty_positions(+Board, ?ListRowCols).
 *   Gets all empty positions on a list.
 */
empty_positions(Board, ListRowCols) :- findall(X, empty_position(Board, X), ListRowCols).

/**
 * valid_moves(+Board, +Player, ?ListOfMoves).
 */
valid_moves(Board, _, ListOfMoves) :- empty_positions(Board, ListOfMoves).

/**
 * move(+[Row, Col], +game(Board, White, Black, P), ?game(Board, White, Black, P)).
 */
move([Row, Col], game(Board, Wc, Bc, w), game(NewBoard, Wc1, Bc, b)) :-
		place_stone(w, Board, [Row, Col], NewBoard, Captures),
		add_captures(Wc, Captures, Wc1).

move([Row, Col], game(Board, Wc, Bc, b), game(NewBoard, Wc, Bc1, w)) :-
		place_stone(b, Board, [Row, Col], NewBoard, Captures),
		add_captures(Bc, Captures, Bc1).



	 	

