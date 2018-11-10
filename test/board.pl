% make_board(Size, Board).
test_make_board :-
    \+ make_board(0, _),
    make_board(1, [[c]]),
    make_board(4, [[c,c,c,c],[c,c,c,c],[c,c,c,c],[c,c,c,c]]),
    make_board(5, [[c,c,c,c,c],[c,c,c,c,c],[c,c,c,c,c],[c,c,c,c,c],[c,c,c,c,c]]).

test_board :- test_all([
    test_make_board
]).

:- test_board.
