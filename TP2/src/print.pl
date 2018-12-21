print_top(_, Columns) :-
    Length is 2 * Columns - 1,
    fill_n(Length, '\x2500\', L),
    atom_chars(S, L),
    write('\x250c\'), write(S), write('\x2510\'), nl.

print_bot(_, Columns) :-
    Length is 2 * Columns - 1,
    fill_n(Length, '\x2500\', L),
    atom_chars(S, L),
    write('\x2514\'), write(S), write('\x2518\'), nl.

print_vertical(_, Numbers, Verticals) :-
    Numbers = [Front|Tail],
    write('\x2502\'), write(Front),
    (   foreach(Number, Tail),
        foreach(Vert, Verticals)
    do  (Vert = 0 -> write('\x2502\'); write(' ')), write(Number)
    ),
    write('\x2502\'), nl.

print_horizontal(_, Horizontals) :-
    Horizontals = [Front|Tail],
    write('\x2502\'), (Front = 0 -> write('\x2500\'); write(' ')),
    (   foreach(Horz, Tail)
    do  write(' '), (Horz = 0 -> write('\x2500\'); write(' '))
    ),
    write('\x2502\'), nl.

print_board(NumbersMatrix, VerticalsMatrix, HorizontalsMatrix) :-
    NumbersMatrix = [FirstNumbers|RestNumbers],
    VerticalsMatrix = [FirstVerticals|RestVerticals],
    length(FirstNumbers, Columns),
    print_top(_, Columns),
    print_vertical(_, FirstNumbers, FirstVerticals),
    (   foreach(Numbers, RestNumbers),
        foreach(Verticals, RestVerticals),
        foreach(Horizontals, HorizontalsMatrix)
    do  print_horizontal(_, Horizontals),
        print_vertical(_, Numbers, Verticals)
    ),
    print_bot(_, Columns).

print_test(0) :-
    print_vertical(_, [1,2,3,4,5,6,7,8,9], [0,0,1,0,1,1,0,0]),
    print_vertical(_, [4,5,6,4,5,6], [0,1,1,0,0]),
    print_horizontal(_, [0,1,0,0,0,1,0,1]).

print_test(1) :-
    print_board([
        [1, 2, 3, 4, 5, 6, 7],
        [2, 2, 3, 4, 5, 6, 7],
        [3, 2, 3, 4, 5, 6, 7],
        [4, 2, 3, 4, 5, 6, 7],
        [5, 6, 7, 8, 9, 6, 7]
    ], [
        [ 0, 1, 0, 0, 1, 0],
        [ 1, 1, 0, 1, 0, 1],
        [ 0, 1, 0, 1, 1, 0],
        [ 0, 0, 1, 0, 0, 0],
        [ 1, 0, 0, 1, 1, 0]
    ], [
        [0, 0, 0, 0, 1, 0, 0],
        [1, 0, 0, 1, 0, 0, 1],
        [0, 0, 1, 0, 0, 1, 0],
        [0, 1, 1, 1, 1, 0, 1]
    ]).
