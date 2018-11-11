% make_board(Size, Board).
test_make_board :-
    \+ make_board(0, _),
    make_board(1, [[c]]),
    make_board(4, [[c,c,c,c],[c,c,c,c],[c,c,c,c],[c,c,c,c]]),
    make_board(5, [[c,c,c,c,c],[c,c,c,c,c],[c,c,c,c,c],[c,c,c,c,c],[c,c,c,c,c]]).

% five_board(Board, P).
test_five_board :-
    five_board([
        [c,c,c,w,c,b,c,c],
        [c,c,c,c,c,w,c,c],
        [c,c,b,b,w,b,w,b],
        [c,c,b,w,b,b,b,c],
        [c,c,w,b,b,w,c,c],
        [c,w,c,b,b,w,b,w],
        [c,b,w,w,w,b,b,b],
        [c,w,b,b,b,w,c,c]], w),
    \+ five_board([
        [c,w,w,w,b,b,b,c],
        [c,c,c,c,c,w,c,c],
        [c,c,b,b,w,b,w,b],
        [c,c,w,w,b,b,b,c],
        [c,c,b,b,b,w,c,c],
        [c,w,c,b,b,w,w,w],
        [c,b,w,w,w,b,b,b],
        [c,w,b,b,b,w,b,b]], w).

% check_dead_stones_*(P, L).
test_check_dead_stones :-
    check_dead_stones_left(w, [c,c,c,b,w,b,b,'W',b,b,c,c]),
    check_dead_stones_left(b, [b,w,w,'B',c]),
    \+ check_dead_stones_left(_, [c,w,w,'B',w,b,b,b,'W',b,b,w,c]),
    check_dead_stones_right(w, [c,c,'W',b,b,w,c,b,b,w]),
    check_dead_stones_right(b, [c,'B',w,w,b,c]),
    \+ check_dead_stones_right(_, [c,b,w,w,'B',w,w,w,b,b,'W',b,b,'W',b,b,c]).

% remove_dead_stones_*(P, Board, [Row, Col], NewBoard, Captures).
test_place_stone :-
    M1 = [
        [w,c,c,c,c,c,w],
        [c,b,c,w,c,b,c],
        [c,c,b,b,b,c,c],
        [b,w,w,c,b,b,w],
        [c,c,b,b,b,c,b],
        [c,b,c,b,c,b,c],
        [w,c,c,w,c,c,b]
    ],
    M1Result = [
        [w,c,c,c,c,c,w],
        [c,c,c,w,c,c,c],
        [c,c,c,b,c,c,c],
        [b,w,w,w,c,c,w],
        [c,c,c,c,b,c,b],
        [c,c,c,c,c,b,c],
        [w,c,c,w,c,c,b]
    ],
    M2 = [
        [c,c,c,c,c,c,c,c,c,c,c,c],
        [c,c,c,c,c,c,b,c,c,c,c,c],
        [c,c,c,c,b,c,w,c,b,c,c,c],
        [c,c,c,b,w,w,w,b,b,c,c,c],
        [c,c,c,b,w,w,c,w,w,w,b,c],
        [w,c,w,c,c,c,w,w,c,c,c,c],
        [w,c,b,c,w,c,w,c,w,w,c,c],
        [b,c,b,b,c,c,w,c,c,b,c,w],
        [c,c,b,w,b,c,w,c,c,b,c,c],
        [c,c,w,w,c,c,b,c,c,w,c,c],
        [c,c,c,c,c,c,c,c,c,c,c,c],
        [c,c,c,c,c,c,c,c,c,c,c,c]
    ],
    M2Result = [
        [c,c,c,c,c,c,c,c,c,c,c,c],
        [c,c,c,c,c,c,b,c,c,c,c,c],
        [c,c,c,c,b,c,c,c,b,c,c,c],
        [c,c,c,b,w,w,c,b,b,c,c,c],
        [c,c,c,b,c,c,b,w,w,w,b,c],
        [w,c,w,c,c,c,w,c,c,c,c,c],
        [w,c,b,c,w,c,w,c,c,w,c,c],
        [b,c,b,b,c,c,w,c,c,b,c,w],
        [c,c,b,w,b,c,w,c,c,b,c,c],
        [c,c,w,w,c,c,b,c,c,w,c,c],
        [c,c,c,c,c,c,c,c,c,c,c,c],
        [c,c,c,c,c,c,c,c,c,c,c,c]
    ],
    place_stone(w, M1, [3,3], M1Result, 10),
    place_stone(b, M2, [4,6], M2Result, 6),
    \+ place_stone(w, M1, [3,3], M1Result, 8).


test_board :- test_all([
    test_make_board,
    test_five_board,
    test_check_dead_stones,
    test_place_stone
]).

:- test_board.
