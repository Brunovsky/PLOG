% Utils
print_test(Test) :- call(Test),
                    format('[OK] ~w~n', Test);
                    format('[FAIL] ~w~n', Test).

test_all(Tests) :- foreach(Tests, print_test).

% Everything in test/
:- reconsult('lists.pl').
:- reconsult('matrix.pl').
:- reconsult('board.pl').
