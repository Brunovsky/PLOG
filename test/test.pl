% Utils
print_test(Test) :- call(Test),
                    format('[OK] ~w~n', Test);
                    format('[FAIL] ~w~n', Test).

test_all(Tests) :- foreach(print_test, Tests).

% Everything in test/
:- reconsult('lists.pl').
:- reconsult('matrix.pl').
:- reconsult('board.pl').

tlists :- test_lists.
tmatrix :- test_matrix.
tboard :- test_board.
tall :- test_lists, test_matrix, test_board.
