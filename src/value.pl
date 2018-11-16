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
score([c,c,w,w,w,c,c], 1).
score([c,c,w,w,w,c], 1).
score([c,c,w,w,w], 1).

score([c,w,w,w,c,c], 1).
score([c,w,w,w,c], 1).
score([c,w,w,w], 1).

score([w,w,w,c,c], 1).
score([w,w,w,c], 1).
score([w,w,w], 1).

% - w w - w -   @   - w - w w -
score([c,w,w,c,w,c], 1).
score([c,w,w,c,w], 1).

score([w,w,c,w,c], 1).
score([w,w,c,w], 1).

score([c,w,c,w,w,c], 1).
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
score([c,w,w,w,w,c], 1). % WIN IN 1
score([c,w,w,w,w], 1).

score([w,w,w,w,c], 1).
score([w,w,w,w], 1).

% - - w w w - w   @   w - w w w - -
score([c,c,w,w,w,c,w], 1).
score([c,w,w,w,c,w], 1).
score([w,w,w,c,w], 1).

score([w,c,w,w,w,c,c], 1).
score([w,c,w,w,w,c], 1).
score([w,c,w,w,w], 1).

% - w - w w - w -
score([c,w,c,w,w,c,w,c], 1).
score([c,w,c,w,w,c,w], 1).

score([w,c,w,w,c,w,c], 1).
score([w,c,w,w,c,w], 1).

% - - w - w - w - w - -
score([c,c,w,c,w,c,w,c,w,c,c], 1).
score([c,c,w,c,w,c,w,c,w,c], 1).
score([c,c,w,c,w,c,w,c,w], 1).

score([c,w,c,w,c,w,c,w,c,c], 1).
score([c,w,c,w,c,w,c,w,c], 1).
score([c,w,c,w,c,w,c,w], 1).

score([w,c,w,c,w,c,w,c,c], 1).
score([w,c,w,c,w,c,w,c], 1).
score([w,c,w,c,w,c,w], 1).

/**
 * score/2. Sequence with 5 stones.
 */

% w w w w w
score([w,w,w,w,w], 2 ** 64). % WIN

% w - w w w - w
score([w,c,w,w,w,c,w], 2 ** 60). % WIN IN 1

% - w w - w - w w -
score([c,w,w,c,w,c,w,w,c], 1).
score([c,w,w,c,w,c,w,w], 1).

score([w,w,c,w,c,w,w,c], 1).
score([w,w,c,w,c,w,w], 1).

% - - w w w - w w   @   w w - w w w - -
score([c,c,w,w,w,c,w,w], 1).
score([c,w,w,w,c,w,w], 1).
score([w,w,w,c,w,w], 1).

score([w,w,c,w,w,w,c,c], 1).
score([w,w,c,w,w,w,c], 1).
score([w,w,c,w,w,w], 1).

% - w - w w - w w   @   w w - w w - w -
score([c,w,c,w,w,c,w,w], 1).
score([w,c,w,w,c,w,w], 1).

score([w,w,c,w,w,c,w,c], 1).
score([w,w,c,w,w,c,w], 1).

% - - w - w - w - w - w - -
score([c,c,w,c,w,c,w,c,w,c,w,c,c], 1).
score([c,c,w,c,w,c,w,c,w,c,w,c], 1).
score([c,c,w,c,w,c,w,c,w,c,w], 1).

score([c,w,c,w,c,w,c,w,c,w,c,c], 1).
score([c,w,c,w,c,w,c,w,c,w,c], 1).
score([c,w,c,w,c,w,c,w,c,w], 1).

score([w,c,w,c,w,c,w,c,w,c,c], 1).
score([w,c,w,c,w,c,w,c,w,c], 1).
score([w,c,w,c,w,c,w,c,w], 1).

% - w - w w - w - w   @   w - w - w w - w -
score([c,w,c,w,w,c,w,c,w], 1).
score([w,c,w,w,c,w,c,w], 1).

score([w,c,w,c,w,w,c,w,c], 1).
score([w,c,w,c,w,w,c,w], 1).

/**
 * score/2. Sequence with 6 stones.
 */

% - - w w w - w w w - -
score([c,c,w,w,w,c,w,w,w,c,c], 1).
score([c,c,w,w,w,c,w,w,w,c], 1).
score([c,c,w,w,w,c,w,w,w], 1).

score([c,w,w,w,c,w,w,w,c,c], 1).
score([c,w,w,w,c,w,w,w,c], 1).
score([c,w,w,w,c,w,w,w], 1).

score([w,w,w,c,w,w,w,c,c], 1).
score([w,w,w,c,w,w,w,c], 1).
score([w,w,w,c,w,w,w], 1).

% w w - w w - w w
score([w,w,c,w,w,c,w,w], 1). % WIN IN 1.

/**
 * score_list/1 dynamic
 * score_list(-List).
 *   Get the list of scored segments.
 */
:- findall(Sequence, score(Sequence, _), List), asserta((score_list(List))).

/**
 * count_score/3
 * count_score(+List, +Sequence, -Score).
 *   Find all instances of Sequence in List, and accumulate the Score.
 */
count_score(List, Sequence, TotalScore) :-
    score(Sequence, Score),
    countsegment(List, Sequence, N),
    TotalScore is Score * N.

/**
 * evaluate/2
 * evaluate(+List, -Value).
 *   Evaluate a list (expensive in time).
 */
evaluate(List, Value) :-
    var(Value), !,
    score_list(Sequences),
    map(count_score(List), Sequences, Scores),
    scanlist(plus, Scores, 0, Value), !.

/**
 * evaluate_matrix/2
 * evaluate_matrix(+Board, -val(Row,Col,Left,Right)).
 *   Evaluate every row, column and diagonal of Board, storing the results
 *   in Row, Col, Left and Right, which are lists enumerating said rows,
 *   columns, left and right diagonals respectively.
 */
evaluate_matrix(Board, Val) :-
    Val = val(RowV,ColV,LeftV,RightV),
    transpose(Board, ColList),
    matrix_left_diagonals(Board, LeftList),
    matrix_right_diagonals(Board, RightList),
    map(evaluate, Board, RowV),
    map(evaluate, ColList, ColV),
    map(evaluate, LeftList, LeftV),
    map(evaluate, RightList, RightV).

/**
 * reevaluate_matrix/3
 * reevaluate_matrix(+[R,C], +Board, +OldVal, -NewVal).
 *   Reevaluate row, column and diagonals passing through (R,C),
 *   as a new piece has been placed there.
 */
reevaluate_matrix([R,C], Board, OldVal, NewVal) :-
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
