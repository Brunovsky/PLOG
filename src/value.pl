/**
 * score/2
 * score(+Sequence, -Score).
 *   Determines the score of a given Sequence.
 */

/**
 * score/2, Sequence with 1 stone.
 */

% - - w - -
score([c,c,w,c,c], 1).
score([c,c,w,c], 1).
score([c,c,w], 1).

score([c,w,c,c], 1).
score([c,w,c], 1).
score([c,w], 1).

score([w,c,c], 1).
score([w,c], 1).
score([w], 1).

/**
 * score/2, Sequence with 2 stones.
 */

% - - w w - -
score([c,c,w,w,c,c], 1).
score([c,c,w,w,c], 1).
score([c,c,w,w], 1).

score([c,w,w,c,c], 1).
score([c,w,w,c], 1).
score([c,w,w], 1).

score([w,w,c,c], 1).
score([w,w,c], 1).
score([w,w], 1).

% - - w - w - -
score([c,c,w,c,w,c,c], 1).
score([c,c,w,c,w,c], 1).
score([c,c,w,c,w], 1).

score([c,w,c,w,c,c], 1).
score([c,w,c,w,c], 1).
score([c,w,c,w], 1).

score([w,c,w,c,c], 1).
score([w,c,w,c], 1).
score([w,c,w], 1).

% - - w - - w - -
score([c,c,w,c,c,w,c,c], 1).
score([c,c,w,c,c,w,c], 1).
score([c,c,w,c,c,w], 1).

score([c,w,c,c,w,c,c], 1).
score([c,w,c,c,w,c], 1).
score([c,w,c,c,w], 1).

score([w,c,c,w,c,c], 1).
score([w,c,c,w,c], 1).
score([w,c,c,w], 1).

% - w - - - w -
score([c,w,c,c,w,c], 1).
score([c,w,c,c,w], 1).

score([w,c,c,w,c], 1).
score([w,c,c,w], 1).

/**
 * score/2. Sequence with 3 stones.
 */

% - - w w w - -
score([c,c,w,w,w,c,c], 1). % SENTE
score([c,c,w,w,w,c], 1). % SENTE
score([c,c,w,w,w], 1).

score([c,w,w,w,c,c], 1). % SENTE
score([c,w,w,w,c], 1).
score([c,w,w,w], 1).

score([w,w,w,c,c], 1).
score([w,w,w,c], 1).
score([w,w,w], 1).

% - w w - w -   @   - w - w w -
score([c,w,w,c,w,c], 1). % SENTE
score([c,w,w,c,w], 1).

score([w,w,c,w,c], 1).
score([w,w,c,w], 1).

score([c,w,c,w,w,c], 1). % SENTE
score([c,w,c,w,w], 1).

score([w,c,w,w,c], 1).
score([w,c,w,w], 1).

% - - w - w - w - -
score([c,c,w,c,w,c,w,c,c], 1).
score([c,c,w,c,w,c,w,c], 1).
score([c,c,w,c,w,c,w], 1).

score([c,w,c,w,c,w,c,c], 1).
score([c,w,c,w,c,w,c], 1).
score([c,w,c,w,c,w], 1).

score([w,c,w,c,w,c,c], 1).
score([w,c,w,c,w,c], 1).
score([w,c,w,c,w], 1).

% - w - - w - w - -   @   - - w - w - - w -
score([c,w,c,c,w,c,w,c,c], 1).
score([c,w,c,c,w,c,w,c], 1).
score([c,w,c,c,w,c,w], 1).

score([w,c,c,w,c,w,c,c], 1).
score([w,c,c,w,c,w,c], 1).
score([w,c,c,w,c,w], 1).

score([c,c,w,c,w,c,c,w,c], 1).
score([c,c,w,c,w,c,c,w], 1).

score([c,w,c,w,c,c,w,c], 1).
score([c,w,c,w,c,c,w], 1).

score([w,c,w,c,c,w,c], 1).
score([w,c,w,c,c,w], 1).

/**
 * score/2. Sequence with 4 stones.
 */

% - w w w w -
score([c,w,w,w,w,c], 1). % WIN IN 1.
score([c,w,w,w,w], 1). % SENTE

score([w,w,w,w,c], 1). % SENTE
score([w,w,w,w], 1).

% - - w w w - w   @   w - w w w - -
score([c,c,w,w,w,c,w], 1). % SENTE 2
score([c,w,w,w,c,w], 1). % SENTE
score([w,w,w,c,w], 1). % SENTE

score([w,c,w,w,w,c,c], 1). % SENTE 2
score([w,c,w,w,w,c], 1). % SENTE
score([w,c,w,w,w], 1). % SENTE

% - w - w w - w -
score([c,w,c,w,w,c,w,c], 1). % SENTE 2
score([c,w,c,w,w,c,w], 1). % SENTE

score([w,c,w,w,c,w,c], 1). % SENTE
score([w,c,w,w,c,w], 1).

% - - w - w - w - w - -
score([c,c,w,c,w,c,w,c,w,c,c], 1). % SENTE
score([c,c,w,c,w,c,w,c,w,c], 1). % SENTE
score([c,c,w,c,w,c,w,c,w], 1). % SENTE

score([c,w,c,w,c,w,c,w,c,c], 1). % SENTE
score([c,w,c,w,c,w,c,w,c], 1). % SENTE
score([c,w,c,w,c,w,c,w], 1). % SENTE

score([w,c,w,c,w,c,w,c,c], 1). % SENTE
score([w,c,w,c,w,c,w,c], 1). % SENTE
score([w,c,w,c,w,c,w], 1). % SENTE

/**
 * score/2. Sequence with 5 stones.
 */

% w w w w w
score([w,w,w,w,w], 2 ** 64). % WIN-

% w - w w w - w
score([w,c,w,w,w,c,w], 2 ** 60). % WIN IN 1.

% - w w - w - w w -
score([c,w,w,c,w,c,w,w,c], 1). % SENTE 2
score([c,w,w,c,w,c,w,w], 1). % SENTE

score([w,w,c,w,c,w,w,c], 1). % SENTE
score([w,w,c,w,c,w,w], 1).

% - - w w w - w w   @   w w - w w w - -
score([c,c,w,w,w,c,w,w], 1). % SENTE 2
score([c,w,w,w,c,w,w], 1). % SENTE
score([w,w,w,c,w,w], 1). % SENTE

score([w,w,c,w,w,w,c,c], 1). % SENTE 2
score([w,w,c,w,w,w,c], 1). % SENTE
score([w,w,c,w,w,w], 1). % SENTE

% - w - w w - w w   @   w w - w w - w -
score([c,w,c,w,w,c,w,w], 1). % SENTE 2
score([w,c,w,w,c,w,w], 1). % SENTE

score([w,w,c,w,w,c,w,c], 1). % SENTE 2
score([w,w,c,w,w,c,w], 1). % SENTE

% - - w - w - w - w - w - -
score([c,c,w,c,w,c,w,c,w,c,w,c,c], 1). % SENTE 3
score([c,c,w,c,w,c,w,c,w,c,w,c], 1). % SENTE 2
score([c,c,w,c,w,c,w,c,w,c,w], 1). % SENTE 2

score([c,w,c,w,c,w,c,w,c,w,c,c], 1). % SENTE 2
score([c,w,c,w,c,w,c,w,c,w,c], 1). % SENTE 2
score([c,w,c,w,c,w,c,w,c,w], 1). % SENTE 2

score([w,c,w,c,w,c,w,c,w,c,c], 1). % SENTE 2
score([w,c,w,c,w,c,w,c,w,c], 1). % SENTE 2
score([w,c,w,c,w,c,w,c,w], 1). % SENTE 2

% - w - w w - w - w   @   w - w - w w - w -
score([c,w,c,w,w,c,w,c,w], 1). % SENTE 2
score([w,c,w,w,c,w,c,w], 1). % SENTE

score([w,c,w,c,w,w,c,w,c], 1). % SENTE 2
score([w,c,w,c,w,w,c,w], 1). % SENTE

/**
 * score/2. Sequence with 6 stones.
 */

% - - w w w - w w w - -
score([c,c,w,w,w,c,w,w,w,c,c], 1). % SENTE 3
score([c,c,w,w,w,c,w,w,w,c], 1). % SENTE 2
score([c,c,w,w,w,c,w,w,w], 1). % SENTE 2

score([c,w,w,w,c,w,w,w,c,c], 1). % SENTE 2
score([c,w,w,w,c,w,w,w,c], 1). % SENTE
score([c,w,w,w,c,w,w,w], 1). % SENTE

score([w,w,w,c,w,w,w,c,c], 1). % SENTE 2
score([w,w,w,c,w,w,w,c], 1). % SENTE
score([w,w,w,c,w,w,w], 1). % SENTE

% w w - w w - w w
score([w,w,c,w,w,c,w,w], 2 ** 60). % WIN IN 1.

% - w - w w - w w w
score([c,w,c,w,w,c,w,w,w], 1). % SENTE 2
score([w,c,w,w,c,w,w,w], 1). % SENTE

score([w,w,w,c,w,w,c,w,c], 1). % SENTE 2
score([w,w,w,c,w,w,c,w], 1). % SENTE

/**
 * dynamic score_list/1
 * score_list(-List).
 *   Get the list of scored segments.
 */
:- findall(Sequence, score(Sequence, _), List), asserta((score_list(List))).

/**
 * multiscore/3
 * multiscore(+List, +Sequence, -Score).
 *   Find all instances of Sequence in List, and accumulate the Score.
 */
multiscore(List, Sequence, TotalScore) :-
    score(Sequence, Score),
    countsegment(List, Sequence, N),
    TotalScore is Score * N.

/**
 * evaluate/2
 * evaluate(+List, -Value).
 *   Evaluate a list (very expensive in terms of time).
 */
evaluate(List, Value) :-
    score_list(Sequences),
    map(multiscore(List), Sequences, Scores), !, % very expensive
    scanlist(plus, Scores, 0, Value), !.

/**
 * ===== ===== BOARD EVALUATIONS ===== =====
 *
 *   There are a few ways to use evaluate/2 to rank board positions.
 * Common among them is the need to evaluate all R rows, all C columns,
 * all R+C-1 left diagonals and all R+C-1 right diagonals, for a
 * total of 3R+3C-2 lists.
 * 
 *   For a 19x19 board this is 112 evaluate/2 calls.
 *   For a 13x13 board this is 76 evaluate/2 calls.
 *   For a 9x9 board this is 52 evaluate/2 calls.
 *   
 *   So we make a predicate evaluate_board/2 which just evaluates a Board
 * in its entirety: all rows, all columns, all diagonals.
 *
 *   We store the Board's value in a compound val/4: val(RowV,ColV,LeftV,RightV),
 * whose args are lists containing the values of the successive rows, column,
 * left diagonals and right diagonals respectively. The diagonals are ordered
 * by matrix_*_diagonal_index/3.
 *
 *   Suppose we have a Board with value V0, and we want to choose the best
 * move for White. Say Board has E empty positions. Then White has E possible
 * moves, some of them pretty stupid, others pretty good, others sometimes forced
 * given the board position. 
 *   Placing a stone in position (R,C) requires reevaluating only row R, column C,
 * and the diagonals passing through (R,C) as long as the move captured no enemy
 * stones. If it did capture enemy stones, we have to reevaluate those now empty
 * positions as well (meaning row, column and diagonals passing through them).
 *
 *   This way we make a predicate reevaluate_board/4 which takes a Board, the
 * position where a new piece has been placed on the Board, the previous
 * val/4 compound, and computes a new val/4 compound by only reevaluating
 * one row, one column, and one of each diagonal. This means a total of four
 * evaluate/2 calls.
 *
 *   Now, we have to do this for each empty position on the Board. Suppose
 * E ~ 80% of the Board, which should be the average.
 *
 *   For a 19x19 board this is 4*0.80*19*19 = 1155 evaluate/2 calls.
 *   For a 13x13 board this is 4*0.80*13*13 = 541 evaluate/2 calls.
 *   For a 9x9 board this is 4*0.80*9*9 = 259 evaluate/2 calls.
 *
 * == Method 1.
 */

/**
 * evaluate_board/2
 * evaluate_board(+Board, -val(Row,Col,Left,Right)).
 *   Evaluate every row, column and diagonal of Board, storing the results
 *   in Row, Col, Left and Right, which are lists enumerating said rows,
 *   columns, left and right diagonals respectively.
 */
evaluate_board(Board, Val) :-
    Val = val(RowV,ColV,LeftV,RightV),
    transpose(Board, ColList),
    matrix_left_diagonals(Board, LeftList),
    matrix_right_diagonals(Board, RightList),
    map(evaluate, Board, RowV),
    map(evaluate, ColList, ColV),
    map(evaluate, LeftList, LeftV),
    map(evaluate, RightList, RightV).

/**
 * reevaluate_board/4
 * reevaluate_board(+[R,C], +Board, +OldVal, -NewVal).
 *   Reevaluate row, column and diagonals passing through (R,C),
 *   as a new piece has been placed there.
 */
reevaluate_board([R,C], Board, OldVal, NewVal) :-
    OldVal = val(RowV,ColV,LeftV,RightV),
    NewVal = val(NewRowV,NewColV,NewLeftV,NewRightV),
    matrix_length(Board, _, ColSize),
    matrix_left_diagonal_index(LeftI, [R,C], ColSize),
    matrix_right_diagonal_index(RightI, [R,C], ColSize),
    matrix_row(R, Board, RowList),
    matrix_col(C, Board, ColList),
    matrix_left_diagonal([R,C], Board, LeftList),
    matrix_right_diagonal([R,C], Board, RightList),
    evaluate(RowList, RowValue),
    evaluate(ColList, ColValue),
    evaluate(LeftList, LeftValue),
    evaluate(RightList, RightValue),
    selectnth1(_, RowV, RowValue, NewRowV, R),
    selectnth1(_, ColV, ColValue, NewColV, C),
    selectnth1(_, LeftV, LeftValue, NewLeftV, LeftI),
    selectnth1(_, RightV, RightValue, NewRightV, RightI).
