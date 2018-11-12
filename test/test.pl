% Utils
print_test(Test) :- call(Test),
                    format('[OK] ~w~n', Test);
                    format('[FAIL] ~w~n', Test).

test_all(Tests) :- foreach(print_test, Tests).

% Everything in test/
:- reconsult('lists.pl').
:- reconsult('matrix.pl').
:- reconsult('board.pl').

tlists :- retest, test_lists.
tmatrix :- retest, test_matrix.
tboard :- retest, test_board.
tall :- retest, test_lists, test_matrix, test_board.

:- test_lists, test_matrix, test_board.
