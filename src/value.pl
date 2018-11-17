/**
 * ===== ===== ===== ===== LIST EVALUATIONS ===== ===== ===== =====
 *
 * To evaluate the board, we will split it into rows, columns and diagonals
 * and evalaute these independently. Each will be called simply a 'list'.
 *
 * Each list is an ordered sequence of {c,w,b} stones. These stones form
 * patterns, which are segments of the list. For example, the list
 *                            [c,b,w,c,c,w,w,c,w,b]
 *                                     ^
 * has patterns [w,w,c,w], [c,c,w], etc, but not [w,w,w] or [b,w,w].
 * This list is good for White: if White plays in the caret position, it becomes
 * a win in 1 (five-in-a-row), regardless of where Black plays. So scoring this
 * list should favor White.
 *
 * We will score patterns independently in favor of White with integer points in
 * the range ]-∞,+∞[, as follows:
 * A winning pattern [w,w,w,w,w] will be worth pretty much +∞, say 2^90.
 * Strong patterns with forcing moves like [c,w,w,w,c,c], threating to make a
 * four-in-a-row (win in 1), will be scored highly, say 2^50 points. The actual
 * pre-win positions like [c,w,w,w,w,c] will be scored even higher, say 2^80 points.
 * Equivalent positions but for Black will be worth the opposite. So position
 * [c,b,b,b,b,c] would be worth -2^80 points.
 *
 * Notice that the list [c,b,b,b,b,c] encloses many patterns, such as [b,b,b],
 * [b,b] and [c,b,b]. Each of these patterns has their own score, which be added up
 * to the score of the pattern [c,b,b,b,b,c] itself to find the value of the list
 * [c,b,b,b,b,c].
 *
 * So we start by scoring patterns comprising only white pieces and empty positions
 * (pure patterns), then consider their reversals (replacing white with black pieces)
 *
 *
 * 
 * ===== ===== ===== ===== BOARD EVALUATIONS ===== ===== ===== =====
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
 *   We store the Board's value in a compound val/4:
 *                       val(RowV,ColV,LeftV,RightV)
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
 *
 * ===== ===== ===== ===== EVALUATION TREE ===== ===== ===== =====
 *
 * Each node in the tree with be represented by the compound node/6:
 *           node(Board, P, val/4, [Wc,Bc], Children, Worth).
 *                                          ^ = [[Ri,Ci]-node/6, ...]
 * P is the next player to play.
 * val/4 is the value compound discussed earlier.
 * Wc and Bc are white and black's captures respectively.
 * Children is a list of Key-Value pairs, where the key is the move.
 * Worth is a float which represents the value of the node; for leaf nodes
 * without children this is computed by totalval/2.
 */

/**
 * ===== ===== ===== ===== =====      ===== ===== ===== ===== =====
 * ===== ===== ===== ===== LIST EVALUATIONS ===== ===== ===== =====
 * ===== ===== ===== ===== =====      ===== ===== ===== ===== =====
 */

/**
 * pattern/2
 * pattern(+Pattern, -Score).
 *   Determines the score of a given pattern.
 */

/**
 * Pure Pattern with 1 stone.
 * ! TODO
 * 9
 */

% - - w - -
pattern([c,c,w,c,c], 1).
pattern([c,c,w,c], 1).
pattern([c,c,w], 1).

pattern([c,w,c,c], 1).
pattern([c,w,c], 1).
pattern([c,w], 1).

pattern([w,c,c], 1).
pattern([w,c], 1).
pattern([w], 1).

/**
 * Pure Pattern with 2 stones.
 * ! TODO
 * 31
 */

% - - w w - -
pattern([c,c,w,w,c,c], 1).
pattern([c,c,w,w,c], 1).
pattern([c,c,w,w], 1).

pattern([c,w,w,c,c], 1).
pattern([c,w,w,c], 1).
pattern([c,w,w], 1).

pattern([w,w,c,c], 1).
pattern([w,w,c], 1).
pattern([w,w], 1).

% - - w - w - -
pattern([c,c,w,c,w,c,c], 1).
pattern([c,c,w,c,w,c], 1).
pattern([c,c,w,c,w], 1).

pattern([c,w,c,w,c,c], 1).
pattern([c,w,c,w,c], 1).
pattern([c,w,c,w], 1).

pattern([w,c,w,c,c], 1).
pattern([w,c,w,c], 1).
pattern([w,c,w], 1).

% - - w - - w - -
pattern([c,c,w,c,c,w,c,c], 1).
pattern([c,c,w,c,c,w,c], 1).
pattern([c,c,w,c,c,w], 1).

pattern([c,w,c,c,w,c,c], 1).
pattern([c,w,c,c,w,c], 1).
pattern([c,w,c,c,w], 1).

pattern([w,c,c,w,c,c], 1).
pattern([w,c,c,w,c], 1).
pattern([w,c,c,w], 1).

% - w - - - w -
pattern([c,w,c,c,w,c], 1).
pattern([c,w,c,c,w], 1).

pattern([w,c,c,w,c], 1).
pattern([w,c,c,w], 1).

/**
 * Pure Pattern with 3 stones.
 * ! TODO
 * 40
 */

% - - w w w - -
pattern([c,c,w,w,w,c,c], 1). % SENTE
pattern([c,c,w,w,w,c], 1). % SENTE
pattern([c,c,w,w,w], 1).

pattern([c,w,w,w,c,c], 1). % SENTE
pattern([c,w,w,w,c], 1).
pattern([c,w,w,w], 1).

pattern([w,w,w,c,c], 1).
pattern([w,w,w,c], 1).
pattern([w,w,w], 1).

% - w w - w -   @   - w - w w -
pattern([c,w,w,c,w,c], 1). % SENTE
pattern([c,w,w,c,w], 1).

pattern([w,w,c,w,c], 1).
pattern([w,w,c,w], 1).

pattern([c,w,c,w,w,c], 1). % SENTE
pattern([c,w,c,w,w], 1).

pattern([w,c,w,w,c], 1).
pattern([w,c,w,w], 1).

% - - w - w - w - -
pattern([c,c,w,c,w,c,w,c,c], 1).
pattern([c,c,w,c,w,c,w,c], 1).
pattern([c,c,w,c,w,c,w], 1).

pattern([c,w,c,w,c,w,c,c], 1).
pattern([c,w,c,w,c,w,c], 1).
pattern([c,w,c,w,c,w], 1).

pattern([w,c,w,c,w,c,c], 1).
pattern([w,c,w,c,w,c], 1).
pattern([w,c,w,c,w], 1).

% - w - - w - w - -   @   - - w - w - - w -
pattern([c,w,c,c,w,c,w,c,c], 1).
pattern([c,w,c,c,w,c,w,c], 1).
pattern([c,w,c,c,w,c,w], 1).

pattern([w,c,c,w,c,w,c,c], 1).
pattern([w,c,c,w,c,w,c], 1).
pattern([w,c,c,w,c,w], 1).

pattern([c,c,w,c,w,c,c,w,c], 1).
pattern([c,c,w,c,w,c,c,w], 1).

pattern([c,w,c,w,c,c,w,c], 1).
pattern([c,w,c,w,c,c,w], 1).

pattern([w,c,w,c,c,w,c], 1).
pattern([w,c,w,c,c,w], 1).

/**
 * Pure Pattern with 4 stones.
 * ! TODO
 * 23
 */

% - w w w w -
pattern([c,w,w,w,w,c], 2 ** 60). % WIN IN 1.
pattern([c,w,w,w,w], 1). % SENTE

pattern([w,w,w,w,c], 1). % SENTE
pattern([w,w,w,w], 1).

% - - w w w - w   @   w - w w w - -
pattern([c,c,w,w,w,c,w], 1). % SENTE 2
pattern([c,w,w,w,c,w], 1). % SENTE
pattern([w,w,w,c,w], 1). % SENTE

pattern([w,c,w,w,w,c,c], 1). % SENTE 2
pattern([w,c,w,w,w,c], 1). % SENTE
pattern([w,c,w,w,w], 1). % SENTE

% - w - w w - w -
pattern([c,w,c,w,w,c,w,c], 1). % SENTE 2
pattern([c,w,c,w,w,c,w], 1). % SENTE

pattern([w,c,w,w,c,w,c], 1). % SENTE
pattern([w,c,w,w,c,w], 1).

% - - w - w - w - w - -
pattern([c,c,w,c,w,c,w,c,w,c,c], 1). % SENTE
pattern([c,c,w,c,w,c,w,c,w,c], 1). % SENTE
pattern([c,c,w,c,w,c,w,c,w], 1). % SENTE

pattern([c,w,c,w,c,w,c,w,c,c], 1). % SENTE
pattern([c,w,c,w,c,w,c,w,c], 1). % SENTE
pattern([c,w,c,w,c,w,c,w], 1). % SENTE

pattern([w,c,w,c,w,c,w,c,c], 1). % SENTE
pattern([w,c,w,c,w,c,w,c], 1). % SENTE
pattern([w,c,w,c,w,c,w], 1). % SENTE

/**
 * Pure Pattern with 5 stones.
 * ! TODO
 * 29
 */

% w w w w w
pattern([w,w,w,w,w], 2 ** 90). % WIN.

% w - w w w - w
pattern([w,c,w,w,w,c,w], 2 ** 60). % WIN IN 1.

% - w w - w - w w -
pattern([c,w,w,c,w,c,w,w,c], 1). % SENTE 2
pattern([c,w,w,c,w,c,w,w], 1). % SENTE

pattern([w,w,c,w,c,w,w,c], 1). % SENTE
pattern([w,w,c,w,c,w,w], 1).

% - - w w w - w w   @   w w - w w w - -
pattern([c,c,w,w,w,c,w,w], 1). % SENTE 2
pattern([c,w,w,w,c,w,w], 1). % SENTE
pattern([w,w,w,c,w,w], 1). % SENTE

pattern([w,w,c,w,w,w,c,c], 1). % SENTE 2
pattern([w,w,c,w,w,w,c], 1). % SENTE
pattern([w,w,c,w,w,w], 1). % SENTE

% - w - w w - w w   @   w w - w w - w -
pattern([c,w,c,w,w,c,w,w], 1). % SENTE 2
pattern([w,c,w,w,c,w,w], 1). % SENTE

pattern([w,w,c,w,w,c,w,c], 1). % SENTE 2
pattern([w,w,c,w,w,c,w], 1). % SENTE

% - - w - w - w - w - w - -
pattern([c,c,w,c,w,c,w,c,w,c,w,c,c], 1). % SENTE 3
pattern([c,c,w,c,w,c,w,c,w,c,w,c], 1). % SENTE 2
pattern([c,c,w,c,w,c,w,c,w,c,w], 1). % SENTE 2

pattern([c,w,c,w,c,w,c,w,c,w,c,c], 1). % SENTE 2
pattern([c,w,c,w,c,w,c,w,c,w,c], 1). % SENTE 2
pattern([c,w,c,w,c,w,c,w,c,w], 1). % SENTE 2

pattern([w,c,w,c,w,c,w,c,w,c,c], 1). % SENTE 2
pattern([w,c,w,c,w,c,w,c,w,c], 1). % SENTE 2
pattern([w,c,w,c,w,c,w,c,w], 1). % SENTE 2

% - w - w w - w - w   @   w - w - w w - w -
pattern([c,w,c,w,w,c,w,c,w], 1). % SENTE 2
pattern([w,c,w,w,c,w,c,w], 1). % SENTE

pattern([w,c,w,c,w,w,c,w,c], 1). % SENTE 2
pattern([w,c,w,c,w,w,c,w], 1). % SENTE

/**
 * Pure Pattern with 6 stones.
 * ! TODO
 * 14
 */

% - - w w w - w w w - -
pattern([c,c,w,w,w,c,w,w,w,c,c], 1). % SENTE 3
pattern([c,c,w,w,w,c,w,w,w,c], 1). % SENTE 2
pattern([c,c,w,w,w,c,w,w,w], 1). % SENTE 2

pattern([c,w,w,w,c,w,w,w,c,c], 1). % SENTE 2
pattern([c,w,w,w,c,w,w,w,c], 1). % SENTE
pattern([c,w,w,w,c,w,w,w], 1). % SENTE

pattern([w,w,w,c,w,w,w,c,c], 1). % SENTE 2
pattern([w,w,w,c,w,w,w,c], 1). % SENTE
pattern([w,w,w,c,w,w,w], 1). % SENTE

% w w - w w - w w
pattern([w,w,c,w,w,c,w,w], 2 ** 60). % WIN IN 1.

% - w - w w - w w w - -
pattern([c,w,c,w,w,c,w,w,w,c,c], 1). % SENTE 3
pattern([c,w,c,w,w,c,w,w,w,c], 1). % SENTE 2
pattern([c,w,c,w,w,c,w,w,w], 1). % SENTE 2

pattern([w,c,w,w,c,w,w,w,c,c], 1). % SENTE 2
pattern([w,c,w,w,c,w,w,w,c], 1). % SENTE
pattern([w,c,w,w,c,w,w,w], 1). % SENTE

pattern([c,c,w,w,w,c,w,w,c,w,c], 1). % SENTE 3
pattern([c,c,w,w,w,c,w,w,c,w], 1). % SENTE 2

pattern([c,w,w,w,c,w,w,c,w,c], 1). % SENTE 2
pattern([c,w,w,w,c,w,w,c,w], 1). % SENTE

pattern([w,w,w,c,w,w,c,w,c], 1). % SENTE 2
pattern([w,w,w,c,w,w,c,w], 1). % SENTE

/**
 * dynamic score/2
 * score(+Pattern, -Score).
 *   Uses pattern/2 to score a given pattern for white or black.
 */
 :- abolish(score/2). % reload
 :- findall([Pattern, Score], pattern(Pattern, Score), List),
    (   foreach([WPattern, WScore], List)
    do  (   list_reversal(WPattern, BPattern),
            BScore is -WScore,
            assertz((score(WPattern, WScore))),
            assertz((score(BPattern, BScore)))
        )
    ).

/**
 * dynamic score_list/1
 * score_list(-PatternList).
 *   Get the list of scored patterns.
 */
 :- abolish(score_list/1). % reload
 :- findall(Pattern, score(Pattern, _), PatternList),
    asserta((score_list(PatternList) :- !)).

/**
 * multiscore/3
 * multiscore(+List, +Pattern, -Score).
 *   Find all instances of Pattern in List, and accumulate their score.
 */
multiscore(List, Pattern, TotalScore) :-
    score(Pattern, Score),
    countsegment(List, Pattern, N),
    TotalScore is Score * N.

/**
 * captures_score/4
 * captures_score(Wc, Bc, Score).
 *   Setting a score to a pair of captures (Wc,Bc).
 */
captures_score([C,C], 0).

captures_score([2,0], 2 ** 25).
captures_score([4,0], 2 ** 37).
captures_score([6,0], 2 ** 52).
captures_score([8,0], 2 ** 77).
captures_score([4,2], 2 ** 20).
captures_score([6,2], 2 ** 49).
captures_score([8,2], 2 ** 76).
captures_score([6,4], 2 ** 44).
captures_score([8,4], 2 ** 74).
captures_score([8,6], 2 ** 69).
captures_score([10,_], 2 ** 90).

captures_score([Wc,Bc], Score) :-
    Wc < Bc,
    captures_score(Bc, Wc, WScore),
    Score is -WScore.

/**
 * dynamic evaluate/2
 * evaluate(+List, -Value).
 *   Evaluate a list.
 */
 :- abolish(evaluate/2). % reload
 :- dynamic evaluate/2.

evaluate(List, Value) :-
    score_list(Patterns),
    map(multiscore(List), Patterns, Scores), !, % very expensive
    scanlist(plus, Scores, 0, Value),
    asserta((evaluate(List, Value))), !. % store for future calls on the same list.

/**
 * ===== ===== ===== ===== ===== ===== ===== ===== ===== ===== =====
 * ===== ===== ===== ===== BOARD EVALUATIONS ===== ===== ===== =====
 * ===== ===== ===== ===== ===== ===== ===== ===== ===== ===== =====
 */

/**
 * sumval/2
 * sumval(+Val, -TotalValue).
 *   Sums up the values in a val/4.
 */
sumval(Val, TotalValue) :-
    Val = val(RowV,ColV,LeftV,RightV),
    sumlist(RowV, RowTotal),
    sumlist(ColV, ColTotal),
    sumlist(LeftV, LeftTotal),
    sumlist(RightV, RightTotal),
    TotalValue is RowTotal + ColTotal + LeftTotal + RightTotal.

/**
 * totalval/4
 * totalval(+Val, +Cap, -TotalValue).
 *   Sums up the values in a val/4 and a cap [Wc,Bc].
 */
totalval(Val, Cap, TotalValue) :-
    sumval(Val, ValValue),
    captures_score(Cap, CapValue),
    TotalValue is ValValue + CapValue.

/**
 * evaluate_board/2
 * evaluate_board(+Board, -Val).
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
 * reevaluate_cell/4
 * reevaluate_cell(+[R,C], +Board, +OldVal, -NewVal).
 *   Reevaluate row, column and diagonals passing through cell (R,C),
 *   as a new piece has been placed there.
 */
reevaluate_cell([R,C], Board, OldVal, NewVal) :-
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
    selectnth1(_, RightV, RightValue, NewRightV, RightI), !.

/**
 * reevaluate_board/5
 * reevaluate_board(+OldBoard, +NewBoard, +OldVal, -NewVal).
 *   Takes a board OldBoard and a new board NewBoard, finds the positions
 *   in which they differ, and computes new val/4 NewVal from OldVal.
 */
reevaluate_board(OldBoard, NewBoard, OldVal, NewVal) :-
    findall(P, matrix_differentnth1(_, OldBoard, _, NewBoard, P), Cells), !,
    (   foreach(Position, Cells),
        fromto(OldVal, InVal, OutVal, NewVal),
        param(NewBoard)
    do  reevaluate_cell(Position, NewBoard, InVal, OutVal)
    ), !.

/**
 * ===== ===== ===== ===== =====     ===== ===== ===== ===== =====
 * ===== ===== ===== ===== EVALUATION TREE ===== ===== ===== =====
 * ===== ===== ===== ===== =====     ===== ===== ===== ===== =====
 */

/**
 * Accessors and other quick utilities.
 */
node_board(node(Board, _, _, _, _, _), Board).
node_next(node(_, P, _, _, _, _), P).
node_val(node(_, _, Val, _, _, _), Val).
node_cap(node(_, _, _, Cap, _, _), Cap).
node_children(node(_, _, _, _, Children, _), Children).
node_worth(node(_, _, _, _, _, Worth), Worth).

child_worth(Worth-(_-_), Worth).
child_move(_-(Move-_), Move).
child_node(_-(_-Child), Child).

/**
 * node_children_worth()
 */

/**
 * node_has_children/1
 * node_has_children(+Node).
 *   True if Node has an already populated children list.
 */


/**
 * node_assert_child/3
 * node_assert_child(+Move, +Node, -Child).
 *   Assert that the next move is Move, discard parent Node and choose its
 *   child node corresponding to Move. If the child node doesn't exist it is created.
 */
node_assert_child(Move, Node, Child) :-
    node_children(Node, Children),
    memberchk(_-(Move-Child), Children),
    !, garbage_collect.

node_assert_child(Move, Node, Child) :-
    build_child_node(Move, Node, Child),
    !, garbage_collect.

/**
 * build_start_node/[2,4]
 * build_start_node(+Board, -Node).
 * build_start_node(+Board, +P, +Cap, -Node).
 *   Build a node/6 from scratch.
 */
build_start_node(Board, Node) :-
    build_start_node(Board, w, [0,0], Node).

build_start_node(Board, P, Cap, Node) :-
    Node = node(Board, P, Val, Cap, [], Total),
    evaluate_board(Board, Val),
    total_val(Val, ValValue),
    captures_score(Cap, CapValue),
    Total is ValValue + CapValue.

/**
 * build_child_node/3
 * build_child_node(+Move, +ParentNode, -ChildNode).
 *   From a ParentNode and a move, constructs a ChildNode.
 */
build_child_node(Move, ParentNode, ChildNode) :-
    other_player(P, Other),
    ParentNode = node(Board, P, Val, Cap, _, _),
    ChildNode = node(ChildBoard, Other, ChildVal, ChildCap, [], TotalValue),
    place_stone(P, Board, Move, ChildBoard, Captures),
    add_captures(P, Captures, Cap, ChildCap),
    reevaluate_board(Board, ChildBoard, Val, ChildVal),
    totalval(Val, Cap, TotalValue).

/**
 * order_children/3
 * order_children(+P, +Children, -OrderedChildren).
 *   Order a list [V-[Ri,Ci]-node/6,...] according to key V.
 */
order_children(w, Children, OrderedChildren) :-
    keysort(Children, BlackOrdered),
    reverse(BlackOrdered, OrderedChildren).

order_children(b, Children, OrderedChildren) :-
    keysort(Children, OrderedChildren).

/**
 * build_children/2
 * build_children(+Node, -NewNode).
 *   Constructs a list [V-([Ri,Ci]-node/6),...] for a Node without children,
 *   no recursion.
 */
build_children(Node, Children) :-
    Node = node(Board, P, Val, Cap, [], Worth),
    NewNode = node(Board, P, Val, Cap, Children, NewWorth),
    node_board(Node, Board),
    empty_positions_within_boundary(Board, 3, ListOfMoves),
    (   foreach(Move, ListOfMoves),
        fromto([], Childs, [Value-(Move-Child)|Childs], Unordered),
        param(Node)
    do  (   build_child_node(Move, Node, Child),
            node_totalval(Child, Value)
        )
    ),
    order_children(P, Unordered, Children),
    head(Children, BestChild),
    child_worth(BestChild, NewWorth).

/**
 * fill_tree/4
 * fill_tree(+Node, )
 */