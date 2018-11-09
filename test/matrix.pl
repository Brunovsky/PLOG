% matrix_size(M, R, C).
test_matrix_size :-
    matrix_size([[1,2,3],[4,5,6]], 2, 3),
    matrix_size([], 0, 0),
    matrix_size([[1,2,3,4,5,6]], 1, 6),
    matrix_size([[1],[2],[a]], 3, 1).

% matrix_get(M, R, C, E).
test_matrix_get :-
    matrix_get([[a,b,c],[d,e,f]], 1, 2, f),
    matrix_get([[a,b],[c,d],[e,f]], 0, 0, a),
    matrix_get([[a,b,c,d],[e,f,g,h]], 0, 1, b).

% matrix_slice(M, R, C, N).
test_matrix_slice :-
    matrix_slice([[a,b,c],[d,e,f],[g,h,i]], [0,2], [1,3], [[b,c],[e,f]]),
    matrix_slice([[a,b,c],[d,e,f],[g,h,i]], 0, 1, [[b,c],[e,f],[h,i]]),
    matrix_slice([[a,b,c],[d,e,f],[g,h,i]], 2, 2, [[i]]),
    matrix_slice([[a,b],[c,d],[e,f],[g,h]], 2, 1, [[f],[h]]),
    matrix_slice([[a,b],[c,d],[e,f],[g,h]], 4, 2, []),
    matrix_slice([], 0, 0, []).

test_matrix :- test_all([
    test_matrix_size,
    test_matrix_get,
    test_matrix_slice
]).

:- test_matrix.
