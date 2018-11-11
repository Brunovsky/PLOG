% matrix_size(M, R, C).
test_matrix_size :-
    matrix_size([[1,2,3],[4,5,6]], 2, 3),
    matrix_size([[1,2,3,4,5,6]], 1, 6),
    matrix_size([[1],[2],[a]], 3, 1).

% matrix_get(M, R, C, E).
test_matrix_get :-
    matrix_get([[a,b,c],[d,e,f]], 2, 3, f),
    matrix_get([[a,b],[c,d],[e,f]], 1, 1, a),
    matrix_get([[a,b,c,d],[e,f,g,h]], 1, 2, b),
    \+ matrix_get([[a,b,c],[d,e,f]], 3, _, _),
    \+ matrix_get([[a,b,c],[d,e,f]], 2, 4, _).

% matrix_set(M, R, C, E, N).
test_matrix_set :-
    matrix_set([[1,2,3],[4,5,6],[a,b,c]], 3, 1, z, [[1,2,3],[4,5,6],[z,b,c]]),
    matrix_set([[1,2,3,4],[a,b,c,d]], 1, 4, z, [[1,2,3,z],[a,b,c,d]]),
    \+ matrix_set([[1,2,3],[4,5,6]], 3, _, _, _),
    \+ matrix_set([[1,2,3],[4,5,6]], 2, 4, _, _).

% matrix_slice(M, R, C, N).
test_matrix_slice :-
    matrix_slice([[a,b,c],[d,e,f],[g,h,i]], [1,2], [2,3], [[b,c],[e,f]]),
    matrix_slice([[a,b,c],[d,e,f],[g,h,i]], 1, 2, [[b,c],[e,f],[h,i]]),
    matrix_slice([[a,b,c],[d,e,f],[g,h,i]], 3, 3, [[i]]),
    matrix_slice([[a,b],[c,d],[e,f],[g,h]], 3, 2, [[f],[h]]),
    matrix_slice([[a,b],[c,d],[e,f],[g,h]], 4, 3, []).

% matrix_main_diag(M, D).
test_matrix_main_diag :-
    matrix_main_diag([[1,2,3],[4,5,6],[7,8,9]], [1,5,9]),
    matrix_main_diag([[1,2,3,4],[5,6,7,8],[9,10,11,12]], [1,6,11]),
    matrix_main_diag([[1,2],[3,4],[5,6],[7,8]], [1,4]),
    matrix_main_diag([[1],[2],[3]], [1]),
    matrix_main_diag([[8,9]],[8]),
    matrix_main_diag([], []).

% matrix_left_diag(M, I, D).
test_matrix_left_diag :-
    matrix_left_diag([[1,2,3],[4,5,6],[7,8,9],[10,11,12]], 0, [1,5,9]),
    matrix_left_diag([[1,2,3],[4,5,6],[7,8,9],[10,11,12]], 1, [4,8,12]),
    matrix_left_diag([[1,2,3],[4,5,6],[7,8,9],[10,11,12]], 2, [7,11]),
    matrix_left_diag([[1,2,3],[4,5,6],[7,8,9],[10,11,12]], -1, [2,6]),
    matrix_left_diag([[1,2,3],[4,5,6],[7,8,9],[10,11,12]], -2, [3]),
    matrix_left_diag([[1,2],[3,4],[5,6],[7,8],[9,10]], 3, [7,10]),
    matrix_left_diag([[1,2,3,4,5],[6,7,8,9,10],[11,12,13,14,15]], 0, [1,7,13]),
    matrix_left_diag([[1,2,3,4,5],[6,7,8,9,10],[11,12,13,14,15]], -2, [3,9,15]),
    matrix_left_diag([[1,2,3,4,5],[6,7,8,9,10],[11,12,13,14,15]], 1, [6,12]).

% matrix_right_diag(M, I, D).
test_matrix_right_diag :-
    matrix_right_diag([[1,2,3],[4,5,6],[7,8,9],[10,11,12]], 0, [3,5,7]),
    matrix_right_diag([[1,2,3],[4,5,6],[7,8,9],[10,11,12]], 1, [6,8,10]),
    matrix_right_diag([[1,2,3],[4,5,6],[7,8,9],[10,11,12]], 2, [9,11]),
    matrix_right_diag([[1,2,3],[4,5,6],[7,8,9],[10,11,12]], -1, [2,4]),
    matrix_right_diag([[1,2,3],[4,5,6],[7,8,9],[10,11,12]], -2, [1]),
    matrix_right_diag([[1,2],[3,4],[5,6],[7,8],[9,10]], 3, [8,9]),
    matrix_right_diag([[1,2,3,4,5],[6,7,8,9,10],[11,12,13,14,15]], 0, [5,9,13]),
    matrix_right_diag([[1,2,3,4,5],[6,7,8,9,10],[11,12,13,14,15]], -2, [3,7,11]),
    matrix_right_diag([[1,2,3,4,5],[6,7,8,9,10],[11,12,13,14,15]], 1, [10,14]).

% matrix_row_reverse(M, R).
test_matrix_row_reverse :-
    matrix_row_reverse([[1,2,3],[4,5,6],[7,8,9]], [[7,8,9],[4,5,6],[1,2,3]]),
    matrix_row_reverse([[1,2],[3,4],[5,6],[7,8]], [[7,8],[5,6],[3,4],[1,2]]),
    matrix_row_reverse([[1,2,3,4,5],[5,6,7,8,9]], [[5,6,7,8,9],[1,2,3,4,5]]).

% matrix_col_reverse(M, R).
test_matrix_col_reverse :-
    matrix_col_reverse([[1,2,3],[4,5,6],[7,8,9]], [[3,2,1],[6,5,4],[9,8,7]]),
    matrix_col_reverse([[1,2],[3,4],[5,6],[7,8]], [[2,1],[4,3],[6,5],[8,7]]),
    matrix_col_reverse([[1,2,3,4,5],[5,6,7,8,9]], [[5,4,3,2,1],[9,8,7,6,5]]).

% matrix_reverse(M, R).
test_matrix_reverse :-
    matrix_reverse([[1,2,3],[4,5,6],[7,8,9]], [[9,8,7],[6,5,4],[3,2,1]]),
    matrix_reverse([[1,2],[3,4],[5,6],[7,8]], [[8,7],[6,5],[4,3],[2,1]]),
    matrix_reverse([[1,2,3,4,5],[5,6,7,8,9]], [[9,8,7,6,5],[5,4,3,2,1]]).

% matrix_transpose(M, T).
test_matrix_transpose :-
    matrix_transpose([[1,2,3],[4,5,6],[7,8,9]], [[1,4,7],[2,5,8],[3,6,9]]),
    matrix_transpose([[1,2],[3,4],[5,6],[7,8]], [[1,3,5,7],[2,4,6,8]]),
    matrix_transpose([[1,2,3,4,5],[5,6,7,8,9]], [[1,5],[2,6],[3,7],[4,8],[5,9]]).

% matrix_left_diagonals(M, Ds).
test_matrix_left_diagonals :-
    matrix_left_diagonals([[1,2,3],[4,5,6],[7,8,9]], [
        [3], [2,6], [1,5,9], [4,8], [7]
    ]),
    matrix_left_diagonals([[1,2],[3,4],[5,6],[7,8],[9,10]], [
        [2], [1,4], [3,6], [5,8], [7,10], [9]
    ]),
    matrix_left_diagonals([[1,2,3,4,5],[6,7,8,9,10],[11,12,13,14,15]], [
        [5], [4,10], [3,9,15], [2,8,14], [1,7,13], [6,12], [11]
    ]).

% matrix_right_diagonals(M, Ds).
test_matrix_right_diagonals :-
    matrix_right_diagonals([[1,2,3],[4,5,6],[7,8,9]], [
        [1], [2,4], [3,5,7], [6,8], [9]
    ]),
    matrix_right_diagonals([[1,2],[3,4],[5,6],[7,8],[9,10]], [
        [1], [2,3], [4,5], [6,7], [8,9], [10]
    ]),
    matrix_right_diagonals([[1,2,3,4,5],[6,7,8,9,10],[11,12,13,14,15]], [
        [1], [2,6], [3,7,11], [4,8,12], [5,9,13], [10,14], [15]
    ]).

% consecutive_any_row(M, E, N).
test_consecutive_any_row :-
    consecutive_any_row([[a,a,b,b,a,b,a],[a,b,b,a,a,a,b],[a,b,b,b,b,a,b]], b, 4),
    consecutive_any_row([[a,b,b,a],[b,b,a,b],[b,b,b,a],[a,a,a,b]], b, 3),
    \+ consecutive_any_row([[a,b,b,b,a],[a,a,a,b,a],[b,a,a,a,b]], a, 4),
    \+ consecutive_any_row([[a,a,b],[b,b,a],[c,c,d],[d,d,d]], c, 3),
    \+ consecutive_any_row([[a,a,b],[b,b,a],[c,c,c],[d,d,d]], c, 4).

% consecutive_any_col(M, E, N).
test_consecutive_any_col :-
    consecutive_any_col([[a,b,a,a],[b,b,a,b],[b,b,b,a],[b,a,b,a]], b, 3),
    consecutive_any_col([[a,b],[c,d],[b,d],[c,a],[c,b],[c,a],[a,d]], c, 3),
    \+ consecutive_any_col([[o,k,k],[k,o,o],[o,o,k]], k, 3),
    \+ consecutive_any_col([[a,b],[a,b],[a,b],[b,a],[a,b],[a,b],[a,b]], a, 4),
    \+ consecutive_any_col([[a,a,b],[b,b,a],[c,c,d],[d,d,d]], c, 5).

% consecutive_any_diag(M, E, N).
test_consecutive_any_diag :-
    consecutive_any_diag([[a,b,b,a],[a,c,b,a],[c,d,c,b],[b,c,a,b]], b, 3),
    \+ consecutive_any_diag([[a,b,b,a],[a,c,b,a],[c,a,a,b],[b,c,c,b]], a, 3).

% sublist_any_row(M, S).
test_sublist_any_row :-
    sublist_any_row([[a,a,b,a,a],[b,b,a,b,c],[d,a,b,c,a]], [a,b,c]),
    sublist_any_row([[a,c,r,q],[l,w,y,a],[w,y,y,l]], [l,w]),
    \+ sublist_any_row([[a,b,c,d],[a,d,c,b],[d,c,b,a]], [d,a]).

% sublist_any_col
test_sublist_any_col :-
    sublist_any_col([[a,a,b,a,a],[b,b,a,b,c],[d,a,b,c,a]], [b,d]),
    sublist_any_col([[a,c,r,q],[l,w,y,a],[w,y,y,l]], [r,y,y]),
    \+ sublist_any_col([[a,b,c,d],[a,d,c,b],[d,c,b,a]], [d,c,b]).

% sublist_any_diag
test_sublist_any_diag :-
    sublist_any_diag([[a,a,b,d,a],[b,b,a,b,c],[d,a,b,c,a]], [d,a,a]),
    sublist_any_diag([[a,c,r,q],[l,w,y,a],[w,y,y,l]], [c,y]),
    \+ sublist_any_diag([[a,b,c,d],[a,d,c,b],[d,c,b,a]], [a,b]).

% matrix_min(M, Min).
test_matrix_min :-
    matrix_min([[4,5,9,3,8,1],[6,0,3,7,8,9]], 0),
    matrix_min([[6,2,7,4,2,7,8],[4,3,6,7,3,9,8]], 2).

% matrix_max(M, Max).
test_matrix_max :-
    matrix_max([[4,5,9,3,8,1],[6,0,3,7,8,9]], 9),
    matrix_max([[6,2,7,4,2,7,8],[4,3,6,7,3,9,8]], 9).


test_matrix :- test_all([
    test_matrix_size,
    test_matrix_get,
    test_matrix_set,
    test_matrix_slice,
    test_matrix_main_diag,
    test_matrix_left_diag,
    test_matrix_right_diag,
    test_matrix_row_reverse,
    test_matrix_col_reverse,
    test_matrix_reverse,
    test_matrix_transpose,
    test_matrix_left_diagonals,
    test_matrix_right_diagonals,
    test_consecutive_any_row,
    test_consecutive_any_col,
    test_consecutive_any_diag,
    test_sublist_any_row,
    test_sublist_any_col,
    test_sublist_any_diag,
    test_matrix_min,
    test_matrix_max
]).

:- test_matrix.
