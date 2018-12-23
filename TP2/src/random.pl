door_prob(0.3).
novalue_prob(0.2).

/**
 * generate_random_board/2
 * generate_random_board(+[R,C], -Board)
 * generates a random RxC Board
 */
generate_random_board([R,C], Board) :-
  R1 is R - 1,
  C1 is C - 1,
  create_matrix(R, C1, Vertical),
  create_matrix(R1, C, Horizontal),
  door_prob(Prob),
  populate_matrix(Vertical, [R,C1], Prob),
  populate_matrix(Horizontal, [R1,C], Prob),
  create_matrix(R, C, Board),
  generate_values(Board, [R,C], Vertical, Horizontal).

/**
* populate_matrix/3
* populate_matrix(+Matrix, +[NR,NC], +Prob)
* populates a given Matrix (NRxNC) with a probability (0 =< Prob =< 1) of being 0
* and the rest is 1
*/
populate_matrix(Matrix, [NR,NC], Prob) :-
  ( for(R, 1, NR),
    param(Matrix),
    param(Prob),
    param(NC)
  do  ( for(C, 1, NC),
        param(Matrix),
        param(Prob),
        param(R)
        do ( matrixnth1([R,C], Matrix, V),
            (maybe(Prob), !, V is 0; V is 1)
           )
      )
  ).

/**
* generate_values/4
* generate_values(+Board, +[NR,NC], +Vertical, +Horizontal)
* generates the board (NRxNC) cells' value with the given Vertical doors and Horizontal doors
* each cell has a probability of having value 0
*/
generate_values(Board, [NR,NC],  Vertical, Horizontal) :-
  novalue_prob(Prob),
  ( for(R, 1, NR),
    param(Prob),
    param(Board),
    param(Vertical),
    param(Horizontal),
    param(NC)
  do  ( for(C, 1, NC),
        param(Prob),
        param(Board),
        param(Vertical),
        param(Horizontal),
        param(R)
        do (
            (maybe(Prob), !, matrixnth1([R,C], Board, 0), !;
            calculate_value(Board, Vertical, Horizontal, [R,C]))
          )
      )
  ).

/**
* calculate_value/4
* calculate_value(+Board, +Vertical, +Horizontal, +[R,C])
*/
calculate_value(Board, Vertical, Horizontal, [R,C]) :-
  matrixnth1([R,C], Board, Value),
  right_total(Vertical, [R,C], Right),
  left_total(Vertical, [R,C], Left),
  top_total(Horizontal, [R,C], Top),
  bot_total(Horizontal, [R,C], Bot),
  Value is Right + Left + Top + Bot + 1.

