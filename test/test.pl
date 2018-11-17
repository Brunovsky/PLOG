% Utils
print_test(Test) :-
    call(Test),
    format('[OK] ~w~n', Test), !;
    format('[FAIL] ~w~n', Test), !.

test_all(Name, Tests) :-
    format('==[TESTS]== ~s', Name),
    nl,
    foreach(print_test, Tests),
    nl.

% Everything in test/
:- reconsult('lists.pl').
:- reconsult('matrix.pl').
:- reconsult('board.pl').
:- reconsult('score.pl').

test :- test_lists, test_matrix, test_board, test_score.
