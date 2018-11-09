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

% matrix_main_diag(M, D).
test_matrix_main_diag :-
    matrix_main_diag([[1,2,3],[4,5,6],[7,8,9]], [1,5,9]),
    matrix_main_diag([[1,2,3,4],[5,6,7,8],[9,10,11,12]], [1,6,11]),
    matrix_main_diag([[1,2],[3,4],[5,6],[7,8]], [1,4]),
    matrix_main_diag([[1],[2],[3]], [1]),
    matrix_main_diag([[8,9]],[8]),
    matrix_main_diag([], []).

/**
 * [  1   2   3 ]         [ 1  2 ]         [  1   2   3   4   5 ]
 * [  4   5   6 ]         [ 3  4 ]         [  6   7   8   9  10 ]
 * [  7   8   9 ]         [ 5  6 ]         [ 11  12  13  14  15 ]
 * [ 10  11  12 ]         [ 7  8 ]
 *                        [ 9 10 ]
 */
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
    matrix_row_reverse([[1,2,3],[4,5,6],[7,8,9]], [[7,8,9],[4,5,6],[1,2,3]]).

% matrix_col_reverse(M, R).
test_matrix_col_reverse :-
    matrix_col_reverse([[1,2,3],[4,5,6],[7,8,9]], [[3,2,1],[6,5,4],[9,8,7]]).

% matrix_reverse(M, R).
test_matrix_reverse :-
    matrix_reverse([[1,2,3],[4,5,6],[7,8,9]], [[9,8,7],[6,5,4],[3,2,1]]).

% matrix_transpose(M, T).
test_matrix_transpose :-
    matrix_transpose([[1,2,3],[4,5,6],[7,8,9]], [[1,4,7],[2,5,8],[3,6,9]]).

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

test_matrix :- test_all([
    test_matrix_size,
    test_matrix_get,
    test_matrix_slice,
    test_matrix_main_diag,
    test_matrix_left_diag,
    test_matrix_right_diag,
    test_matrix_row_reverse,
    test_matrix_col_reverse,
    test_matrix_reverse,
    test_matrix_transpose,
    test_matrix_left_diagonals,
    test_matrix_right_diagonals
]).

:- test_matrix.
