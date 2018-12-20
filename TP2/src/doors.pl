doors(Board, L) :-
  length(L, 3),
  domain(L, 0, 1),
  line(Board, L),
  labeling([ffc], L).

line([_|[]], _).
line([H|T], [H1|T1]) :-
  calculate_value([H1|T1], V),
  H #= V+1,
  line(T, T1).



calculate_value([H|[]], H).
calculate_value([H|T], V) :-
  calculate_value(T, V1),
  V #= H + H*V1.



% generate_matrix(Rows, Cols, Matrix) :-
%   length(Matrix, Rows),
%   maplist(length(Cols), Matrix).

horizontal_right(Board, Size, [R,C], L) :-
  matrix_range(Board, [L|_], [[R,R],[C,Size]]).

horizontal_left(Board, [R,C], L) :-
  matrix_range(Board, [T|_], [[R,R],[1,C]]),
  reverse(T, L).

vertical_top(Board, [R,C], L) :-
  matrix_range(Board, M, [[1,R],[C,C]]),
  flatten(M, T),
  reverse(T, L).

vertical_bottom(Board, Size, [R,C], L) :-
  matrix_range(Board, M, [[R,Size],[C,C]]),
  flatten(M, L).



test :-
  doors([2,2,2,2],L) %[1,0,1]
