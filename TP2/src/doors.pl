test :- doors([[4,2,4,2],[5,3,5,3],[4,3,4,4],[2,3,2,4]]).

doors(Board) :-
  length(Board, NRows),
  NRows1 is NRows - 1,
  nth1(1, Board, Line),
  length(Line, NCols),
  NCols1 is NCols - 1,
  create_matrix(NRows, NCols1, Vertical),
  create_matrix(NRows1, NCols, Horizontal),
  get_solution(Board, [NRows,NCols], Vertical, Horizontal, [1,1]),
  labeling([ff], Vertical).


length_list(N, L) :- length(L, N).
create_matrix(R, C, M) :-
  length_list(R, M),
  maplist(length_list(C), M).

line([_], _).
line([H|T], [H1|T1]) :-
  calculate_value([H1|T1], V),
  H #= V+1,
  line(T, T1).

calculate_value([H], H).
calculate_value([H|T], V) :-
  calculate_value(T, V1),
  V #= H + H*V1.

%gets the Ith column of a matrix, 1-indexed
get_column(_, [], C, C).
get_column(I, [H|T], C, R) :-
  nth1(I, H, E),
  append(C, [E], C1),
  get_column(I, T, C1, R).

get_solution(_, [NR,NC], _, _, [NR,C]) :- C #= NC+1.
get_solution(Board, [NR,NC], Vertical, Horizontal, [R,C]) :-
  matrixnth1([R|[C]], Board, Value),
  right_total(Vertical, [R,C], Right),
  left_total(Vertical, [R,C], Left),
  top_total(Horizontal, [R,C], Top),
  bot_total(Horizontal, [R,C], Bot),
  Right + Left + Top + Bot + 1 #= Value,
  (C1 #= C+1,
    C #< NC,
    get_solution(Board, [NR,NC], Vertical, Horizontal, [R,C1]);
  R1 #= R+1,
  get_solution(Board, [NR,NC], Vertical, Horizontal, [R1,1])).

right_total(Vertical, [R,C], Total) :-
  nth1(R, Vertical, L),
  domain(L, 0, 1),
  proper_length(L, S),
  range(L, Sublist, [C,S]),
  calculate_value(Sublist, Total).
right_total(_,_,0).

left_total(Vertical, [R,C], Total) :-
  nth1(R, Vertical, L),
  domain(L, 0, 1),
  range(L, Sublist1, [1,C]),
  reverse(Sublist1, Sublist),
  calculate_value(Sublist, Total).
left_total(_,_,0).

top_total(Horizontal, [R,C], Total) :-
  get_column(C, Horizontal, [], L),
  domain(L, 0, 1),
  range(L, Sublist1, [1,R]),
  reverse(Sublist1, Sublist),
  calculate_value(Sublist, Total).
left_total(_,_,0).

bot_total(Horizontal, [R,C], Total) :-
  get_column(C, Horizontal, [], L),
  domain(L, 0, 1),
  proper_length(L, S),
  range(L, Sublist, [R,S]),
  calculate_value(Sublist, Total).
bot_total(_,_,0).
