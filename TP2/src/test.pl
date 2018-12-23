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


test_prob :-
  doors([
    [4,2,4,2],
    [5,3,5,3],
    [4,3,4,4],
    [2,3,2,4]
  ]).
