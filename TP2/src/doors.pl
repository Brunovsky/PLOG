test :- doors([[4,2,4,2],[5,3,5,3],[4,3,4,4],[2,3,2,4]]).

doors(Board) :-
  length(Board, NRows),
  NRows1 is NRows - 1,
  nth1(1, Board, Line),
  length(Line, NCols),
  NCols1 is NCols - 1,
  create_matrix(NRows, NCols1, Vertical),
  create_matrix(NRows1, NCols, Horizontal),
  % Use 1 for door present and 0 for door absent
  flatten(Vertical, VerticalFlat),
  flatten(Horizontal, HorizontalFlat),
  append(VerticalFlat, HorizontalFlat, Vars),
  domain(Vars, 0, 1),
  % Apply restriction on each cell
  (   for(R, 1, NRows),
      param(Board),
      param(Vertical),
      param(Horizontal),
      param(NCols)
  do  ( for(C, 1, NCols),
        param(Board),
        param(Vertical),
        param(Horizontal),
        param(R)
      do get_solution(Board, Vertical, Horizontal, [R,C])
      )
  ),
  labeling([ff], Vars),
  print_board(Board, Vertical, Horizontal).

length_list(N, L) :- length(L, N).
create_matrix(R, C, M) :-
  length_list(R, M),
  maplist(length_list(C), M).

calculate_value([], 0).
calculate_value([H|T], V) :-
  calculate_value(T, V1),
  V #= H + H*V1.

/**
 * get_solution/4
 * get_solution(+Board, +Vertical, +Horizontal, +Cell).
 * Computes the four accumulators on cell [R,C], and bind them
 * to the value of the respective cell in the Board.
 */
get_solution(Board, Vertical, Horizontal, [R,C]) :-
  matrixnth1([R,C], Board, Value),
  right_total(Vertical, [R,C], Right),
  left_total(Vertical, [R,C], Left),
  top_total(Horizontal, [R,C], Top),
  bot_total(Horizontal, [R,C], Bot),
  Right + Left + Top + Bot + 1 #= Value.

/**
 * right_total/3
 * right_total(+Vertical, +[R,C], -Total).
 * Compute the accumulator function to the right of cell [R,C].
 */
right_total(Vertical, [R,C], Total) :-
  nth1(R, Vertical, L),
  proper_length(L, S),
  range(L, Sublist, [C,S]),
  calculate_value(Sublist, Total).

/**
 * left_total/3
 * left_total(+Vertical, +[R,C], -Total).
 * Compute the accumulator function to the left of cell [R,C].
 */
left_total(Vertical, [R,C], Total) :-
  nth1(R, Vertical, L),
  range(L, Sublist1, [1,C]),
  reverse(Sublist1, Sublist),
  calculate_value(Sublist, Total).

/**
 * top_total/3
 * top_total(+Vertical, +[R,C], -Total).
 * Compute the accumulator function upwards of cell [R,C].
 */
top_total(Horizontal, [R,C], Total) :-
  matrix_col(C, Horizontal, L),
  range(L, Sublist1, [1,R]),
  reverse(Sublist1, Sublist),
  calculate_value(Sublist, Total).

/**
 * bot_total/3
 * bot_total(+Vertical, +[R,C], -Total).
 * Compute the accumulator function downwards of cell [R,C].
 */
bot_total(Horizontal, [R,C], Total) :-
  matrix_col(C, Horizontal, L),
  proper_length(L, S),
  range(L, Sublist, [R,S]),
  calculate_value(Sublist, Total).



test_calculate_value :-
  calculate_value([], A1), write(A1), nl,
  calculate_value([1,0,1,0,1], A2), write(A2), nl,
  calculate_value([1,1,1,0,1], A3), write(A3), nl,
  calculate_value([0,1,1,1,1], A4), write(A4), nl,
  calculate_value([1,1,0,0,1,0,0], A5), write(A5), nl,
  calculate_value([1,0,0,0,0], A6), write(A6), nl,
  B = [B1,B2,B3,B4,B5,B6,B7],
  domain(B, 0, 1),
  calculate_value(B, 5),
  labeling([ff], B),
  write(B), nl, fail; otherwise.
