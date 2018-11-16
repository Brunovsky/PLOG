/**
 * make_board/2
 * make_board(+Size, ?Board).
 *   Constructs a SizexSize board with no pieces (all c) to output Board.
 */
make_board(Size, Board) :- Size > 0, fill_n(Size, c, Row), fill_n(Size, Row, Board).

/**
 * board_size/2
 * board_size(+Board, ?Size).
 */
board_size(Board, Size) :- matrix_proper_length(Board, Size, Size).

/**
 * player_reversal/2
 * player_reversal(?WBoard, ?BBoard).
 *   Changes white pieces to black pieces and vice versa.
 */
player_reversal(WBoard, BBoard) :- map(other_player, WBoard, BBoard).

/**
 * five_board/2
 * five_board(+Board, +P).
 *   Verifies if the Board position has a five-in-a-row for player P.
 */
five_board(Board, P) :- consecutive_matrix(Board, P, 5).

/**
 * game_over/2
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
 * check_dead_stones_left/2
 * check_dead_stones_left(+P, +L).
 *   Remember: suicides are not allowed! That's what the caps
 *   trick is for, and why we have to check left and right.
 */
check_dead_stones_left(w, L) :- segment(L, [w,b,b,'W']).
check_dead_stones_left(b, L) :- segment(L, [b,w,w,'B']).

/**
 * check_dead_stones_right/2
 * check_dead_stones_right(+P, +L).
 *   Remember: suicides are not allowed! That's what the caps
 *   trick is for, and why we have to check left and right.
 */
check_dead_stones_right(w, L) :- segment(L, ['W',b,b,w]).
check_dead_stones_right(b, L) :- segment(L, ['B',w,w,b]).

/**
 * remove_dead_stones_rowleft/5
 * remove_dead_stones_rowleft(+P, +Board, +[R,C], ?NewBoard, ?Captures).
 *   Remove dead stones on the row, left of (R,C), killed by P.
 */
remove_dead_stones_rowleft(P, Board, [R,_], Board, 0) :-
    matrix_row(R, Board, RowList),
    \+ check_dead_stones_left(P, RowList).

remove_dead_stones_rowleft(P, Board, [R,C], NewBoard, 2) :-
    matrix_row(R, Board, RowList),
    check_dead_stones_left(P, RowList),
    C1 is C - 1, C2 is C - 2,
    matrix_selectnth1(_, Board, c, Inter, [R,C1]),
    matrix_selectnth1(_, Inter, c, NewBoard, [R,C2]).

/**
 * remove_dead_stones_rowright/5
 * remove_dead_stones_rowright(+P, +Board, +[R,C], ?NewBoard, ?Captures).
 *   Remove dead stones on the row, right of (R,C), killed by P.
 */
remove_dead_stones_rowright(P, Board, [R,_], Board, 0) :-
    matrix_row(R, Board, RowList),
    \+ check_dead_stones_right(P, RowList).

remove_dead_stones_rowright(P, Board, [R,C], NewBoard, 2) :-
    matrix_row(R, Board, RowList),
    check_dead_stones_right(P, RowList),
    C1 is C + 1, C2 is C + 2,
    matrix_selectnth1(_, Board, c, Inter, [R,C1]),
    matrix_selectnth1(_, Inter, c, NewBoard, [R,C2]).

/**
 * remove_dead_stones_colabove/5
 * remove_dead_stones_colabove(+P, +Board, +[R,C], ?NewBoard, ?Captures).
 *   Remove dead stones on the column, above of (R,C), killed by P.
 */
remove_dead_stones_colabove(P, Board, [_,C], Board, 0) :-
    matrix_col(C, Board, ColList),
    \+ check_dead_stones_left(P, ColList).

remove_dead_stones_colabove(P, Board, [R,C], NewBoard, 2) :-
    matrix_col(C, Board, ColList),
    check_dead_stones_left(P, ColList),
    R1 is R - 1, R2 is R - 2,
    matrix_selectnth1(_, Board, c, Inter, [R1,C]),
    matrix_selectnth1(_, Inter, c, NewBoard, [R2,C]).

/**
 * remove_dead_stones_colbelow/5
 * remove_dead_stones_colbelow(+P, +Board, +[R,C], ?NewBoard, ?Captures).
 *   Remove dead stones on the column, below of (R,C), killed by P.
 */
remove_dead_stones_colbelow(P, Board, [_,C], Board, 0) :-
    matrix_col(C, Board, ColList),
    \+ check_dead_stones_right(P, ColList).

remove_dead_stones_colbelow(P, Board, [R,C], NewBoard, 2) :-
    matrix_col(C, Board, ColList),
    check_dead_stones_right(P, ColList),
    R1 is R + 1, R2 is R + 2,
    matrix_selectnth1(_, Board, c, Inter, [R1,C]),
    matrix_selectnth1(_, Inter, c, NewBoard, [R2,C]).

/**
 * remove_dead_stones_leftleft/5
 * remove_dead_stones_leftleft(+P, +Board, +[R,C], ?NewBoard, ?Captures).
 *   Remove dead stones on the left diagonal, left of (R,C), killed by P.
 */
remove_dead_stones_leftleft(P, Board, [R,C], Board, 0) :-
    matrix_left_diagonal([R,C], Board, DiagonalList),
    \+ check_dead_stones_left(P, DiagonalList).

remove_dead_stones_leftleft(P, Board, [R,C], NewBoard, 2) :-
    matrix_left_diagonal([R,C], Board, DiagonalList),
    check_dead_stones_left(P, DiagonalList),
    R1 is R - 1, R2 is R - 2,
    C1 is C - 1, C2 is C - 2,
    matrix_selectnth1(_, Board, c, Inter, [R1,C1]),
    matrix_selectnth1(_, Inter, c, NewBoard, [R2,C2]).

/**
 * remove_dead_stones_leftright/5
 * remove_dead_stones_leftright(+P, +Board, +[R,C], ?NewBoard, ?Captures).
 *   Remove dead stones on the left diagonal, right of (R,C), killed by P.
 */
remove_dead_stones_leftright(P, Board, [R,C], Board, 0) :-
    matrix_left_diagonal([R,C], Board, DiagonalList),
    \+ check_dead_stones_right(P, DiagonalList).

remove_dead_stones_leftright(P, Board, [R,C], NewBoard, 2) :-
    matrix_left_diagonal([R,C], Board, DiagonalList),
    check_dead_stones_right(P, DiagonalList),
    R1 is R + 1, R2 is R + 2,
    C1 is C + 1, C2 is C + 2,
    matrix_selectnth1(_, Board, c, Inter, [R1,C1]),
    matrix_selectnth1(_, Inter, c, NewBoard, [R2,C2]).

/**
 * remove_dead_stones_rightleft/5
 * remove_dead_stones_rightleft(+P, +Board, +[R,C], ?NewBoard, ?Captures).
 *   Remove dead stones on the right diagonal, left of (R,C), killed by P.
 */
remove_dead_stones_rightleft(P, Board, [R,C], Board, 0) :-
    matrix_right_diagonal([R,C], Board, DiagonalList),
    \+ check_dead_stones_left(P, DiagonalList).

remove_dead_stones_rightleft(P, Board, [R,C], NewBoard, 2) :-
    matrix_right_diagonal([R,C], Board, DiagonalList),
    check_dead_stones_left(P, DiagonalList),
    R1 is R - 1, R2 is R - 2,
    C1 is C + 1, C2 is C + 2,
    matrix_selectnth1(_, Board, c, Inter, [R1,C1]),
    matrix_selectnth1(_, Inter, c, NewBoard, [R2,C2]).

/**
 * remove_dead_stones_rightright/5
 * remove_dead_stones_rightright(+P, +Board, +[R,C], ?NewBoard, ?Captures).
 *   Remove dead stones on the right diagonal, right of (R,C), killed by P.
 */
remove_dead_stones_rightright(P, Board, [R,C], Board, 0) :-
    matrix_right_diagonal([R,C], Board, DiagonalList),
    \+ check_dead_stones_right(P, DiagonalList).

remove_dead_stones_rightright(P, Board, [R,C], NewBoard, 2) :-
    matrix_right_diagonal([R,C], Board, DiagonalList),
    check_dead_stones_right(P, DiagonalList),
    R1 is R + 1, R2 is R + 2,
    C1 is C - 1, C2 is C - 2,
    matrix_selectnth1(_, Board, c, Inter, [R1,C1]),
    matrix_selectnth1(_, Inter, c, NewBoard, [R2,C2]).

/**
 * remove_dead_stones/5
 * remove_dead_stones(+P, +Board, +[R,C], ?NewBoard, ?Captures).
 *   Remove all dead stones (rows, columns and diagonals).
 *   Remember: suicides are not allowed!
 */
remove_dead_stones(P, Board, [R,C], NewBoard, Captures) :-
    remove_dead_stones_rowleft(P, Board, [R,C], New1, C1),
    remove_dead_stones_rowright(P, New1, [R,C], New2, C2),
    remove_dead_stones_colabove(P, New2, [R,C], New3, C3),
    remove_dead_stones_colbelow(P, New3, [R,C], New4, C4),
    remove_dead_stones_leftleft(P, New4, [R,C], New5, C5),
    remove_dead_stones_leftright(P, New5, [R,C], New6, C6),
    remove_dead_stones_rightleft(P, New6, [R,C], New7, C7),
    remove_dead_stones_rightright(P, New7, [R,C], NewBoard, C8),
    Captures is C1 + C2 + C3 + C4 + C5 + C6 + C7 + C8.

/**
 * place_stone/5
 * place_stone(+P, +Board, +[R,C], ?NewBoard, ?Captures).
 *   Places a stone P (w or b) on Board at position (R,C).
 *   Doing so removes captured stones from the board, binding the
 *   number of removed stones to Captures.
 */
place_stone(w, Board, [R,C], NewBoard, Captures) :-
	matrix_selectnth1(c, Board, 'W', WBoard, [R,C]),
    remove_dead_stones(w, WBoard, [R,C], Removed, Captures),
    matrix_selectnth1('W', Removed, w, NewBoard, [R,C]).

place_stone(b, Board, [R,C], NewBoard, Captures) :-
    matrix_selectnth1(c, Board, 'B', BBoard, [R,C]),
    remove_dead_stones(b, BBoard, [R,C], Removed, Captures),
    matrix_selectnth1('B', Removed, b, NewBoard, [R,C]).

/**
 * empty_position/2
 * empty_position(+Board, ?[R,C]).
 *   Asserts (R,C) is an empty position (c) on the Board.
 *   Provides all such positions.
 */
empty_position(Board, [R,C]) :- matrixnth1([R,C], Board, c).

/**
 * empty_positions/2
 * empty_positions(+Board, ?ListRowCols).
 *   Gets all empty positions on a list.
 */
empty_positions(Board, ListRowCols) :- findall(X, empty_position(Board, X), ListRowCols).

/**
 * valid_moves/3
 * valid_moves(+Board, +Player, ?ListOfMoves).
 *   Gets a list with all the valid moves (does not depend on the player).
 */
valid_moves(Board, _, ListOfMoves) :- empty_positions(Board, ListOfMoves).

/**
 * move/3
 * move(+[R,C], +game(Board, Wc, Bc, P), -game(Board, Wc, Bc, P)).
 */
move([R,C], game(Board, Wc, Bc, w), game(NewBoard, Wc1, Bc, b)) :-
    place_stone(w, Board, [R,C], NewBoard, Captures),
    add_captures(Wc, Captures, Wc1).

move([R,C], game(Board, Wc, Bc, b), game(NewBoard, Wc, Bc1, w)) :-
    place_stone(b, Board, [R,C], NewBoard, Captures),
    add_captures(Bc, Captures, Bc1).
