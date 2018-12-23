reset_timer :- statistics(walltime, _).
print_time :-
  statistics(walltime, [_,T]),
  TS is ((T//10)*10)/1000,
  nl, format('Time: ~5fs ~n~n', [TS]).
