/**
 * composed_pattern/3
 * composed_pattern(?[Wc,Bc], +Pattern, -Score).
 *   Determines the score of a given composed pattern.
 */

% Auxiliary
compslist(0, [H|_], H).
compslist(2, [_,H|_], H).
compslist(4, [_,_,H|_], H).
compslist(6, [_,_,_,H|_], H).
compslist(8, [_,_,_,_,H|_], H).

/**
 * Fork patterns.
 * ! TODO
 */
% b w b - -
composed_pattern([Wc,_], [b,w,b,c,c], Score) :-
    compslist(Wc, [2 ** 8, 2 ** 15, 2 ** 23, 2 ** 32, 2 ** 45], Score).

composed_pattern([Wc,_], [c,c,b,w,b}, Score) :-
    compslist(Wc, [2 ** 8, 2 ** 15, 2 ** 23, 2 ** 32, 2 ** 45], Score).

% b w w b - -
composed_pattern([Wc,_], [b,w,w,b,c,c], Score) :-
    compslist(Wc, [2 ** 9, 2 ** 16, 2 ** 25, 2 ** 34, 2 ** 47], Score).

composed_pattern([Wc,_], [c,c,b,w,w,b}, Score) :-
    compslist(Wc, [2 ** 9, 2 ** 16, 2 ** 25, 2 ** 34, 2 ** 47], Score).

% w w w b - -
composed_pattern([Wc,_], [w,w,w,b,c,c], Score) :-
    compslist(Wc, [2 ** 16, 2 ** 29, 2 ** 27, 2 ** 35, 2 ** 48], Score).

composed_pattern([Wc,_], [c,c,b,w,w,w], Score) :-
    compslist(Wc, [2 ** 16, 2 ** 29, 2 ** 27, 2 ** 35, 2 ** 48], Score).





/**
 * matrix_size(+M, ?R, ?C).
 *   The matrix M is made up of R rows and C columns.
 *   M is a list of lists, all of the same length,
 *   otherwise this predicate fails.
 *
 * In the rest of this file, M denotes a matrix, R a row, and C a column.
 */
matrix_size([], 0, 0).
matrix_size(M, R, C) :- proper_length(M, R), a_all_of(M, proper_length, C).

/**
 * is_matrix(+M).
 *   Asserts M is a rectangular matrix.
 */
is_matrix(M) :- matrix_size(M, _, _).

/**
 * matrix_nth0(?R, ?C, ?M, ?Elem).
 *   Elem is the element at position (R,C) in matrix M.
 *   Either M is proper or R and C are integers.
 */
matrix_nth0(R, C, M, Elem) :- nth0(R, M, L), nth0(C, L, Elem).

/**
 * matrix_nth1(?R, ?C, ?M, ?Elem).
 *   Elem is the element at position (R,C) in matrix M.
 *   Either M is proper or R and C are integers.
 */
matrix_nth1(R, C, M, Elem) :- nth1(R, M, L), nth1(C, L, Elem).

/**
 * matrix_set(+M, +R, +C, +E, -N).
 *   Sets E at position (R,C) on matrix M, with result N.
 */
matrix_set(M, R, C, E, N) :- nth1(R, M, ListRow),
                             list_set(ListRow, C, E, NewRow),
                             list_set(M, R, NewRow, N).

/**
 * matrix_row(+M, +R, ?L).
 *   L is the row R of matrix M.
 */
matrix_row(M, R, L) :- list_get(M, R, L).

/**
 * matrix_col(+M, +C, ?L).
 *   L is the column C of matrix M.
 */
matrix_col(M, C, L) :- l_map(M, list_get, [C], L).

/**
 * matrix_slice(+M, +Row, +Col, ?N).
 * matrix_slice(+M, +Row, +[ColBegin, ColEnd], ?N).
 * matrix_slice(+M, [RowBegin, RowEnd], +Col, ?N).
 * matrix_slice(+M, +[RowBegin, RowEnd], +[ColBegin, ColEnd], ?N).
 *   Extract a submatrix from M, starting at row RowBegin (inclusive)
 *   and ending at row RowEnd (inclusive), idem for columns.
 */
matrix_slice(M, Row, Col, N) :-
    range(M, Row, M1),
    a_map(M1, range, Col, M2),
    clear_empty_list(M2, N).
/*
    integer(Row), integer(Col),
    matrix_size(M, R, C),
    matrix_slice(M, [Row, R], [Col, C], N).

matrix_slice(M, Row, [ColBegin, ColEnd], N) :-
    integer(Row),
    matrix_size(M, R, _),
    matrix_slice(M, [Row, R], [ColBegin, ColEnd], N).

matrix_slice(M, [RowBegin, RowEnd], Col, N) :-
    integer(Col),
    matrix_size(M, _, C),
    matrix_slice(M, [RowBegin, RowEnd], [Col, C], N).

matrix_slice(M, [RowBegin, RowEnd], [ColBegin, ColEnd], N) :-
    range(M, [RowBegin, RowEnd], M1),
    l_map(M1, range, [[ColBegin, ColEnd]], M2),
    clear_empty_list(M2, N). % [[],[],[]] --> []
*/

/**
 * matrix_main_diag(+M, ?D).
 *   Extracts the main diagonal from matrix M.
 */
matrix_main_diag([], []).
matrix_main_diag(M, [E | T]) :- matrix_get(M, 1, 1, E),
                                matrix_slice(M, 2, 2, N),
                                matrix_main_diag(N, T).

/**
 * matrix_left_diagonal(+M, +I, ?D).
 *   Extracts the main diagonal D from a submatrix of M.
 *   If I is 0, it is the main diagonal of M.
 *   If I > 0, it is the diagonal I spaces below it.
 *   If I < 0, it is the diagonal I spaces above it.
 *   The elements of D are ordered by row.
 */
matrix_left_diag(M, 0, D) :- matrix_main_diag(M, D).
matrix_left_diag(M, I, D) :- I < 0, J is 1 - I,
                             matrix_slice(M, 1, J, N),
                             matrix_main_diag(N, D);
                             I > 0, J is I + 1,
                             matrix_slice(M, J, 1, N),
                             matrix_main_diag(N, D).

/**
 * matrix_right_diagonal(+M, +I, ?D).
 *   Extracts the diagonal perpendicular to the main diagonal
 *   from a submatrix of M.
 *   If I is 0, it is the diagonal starting at the top right corner of M.
 *   If I > 0, it is the diagonal I spaces below it.
 *   If I < 0, it is the diagonal I spaces above it.
 *   The elements of D are ordered by row.
 */
matrix_right_diag(M, I, D) :- matrix_col_reverse(M, R),
                              matrix_left_diag(R, I, D).

/**
 * matrix_left_diag_through(+M, +Row, +Col, ?D).
 *   Extracts the left diagonal passing through (Row, Col).
 */
matrix_left_diag_through(M, Row, Col, D) :- I is Row - Col,
                                            matrix_left_diag(M, I, D).

/**
 * matrix_right_diag_through(+M, +Row, +Col, ?D).
 *   Extracts the right diagonal passing through (Row, Col).
 */
matrix_right_diag_through(M, Row, Col, D) :- matrix_size(M, _, ColSize),
                                             I is Row + Col - ColSize - 1,
                                             matrix_right_diag(M, I, D).

/**
 * matrix_through(+M, +Row, +Col, ?Ls).
 *   Extracts the row, column, and two diagonals passing through (Row, Col).
 */
matrix_through(M, Row, Col, [L1, L2, L3, L4]) :- matrix_row(M, Row, L1),
                                                 matrix_col(M, Col, L2),
                                                 matrix_left_diag(M, Row, Col, L3),
                                                 matrix_right_diag(M, Row, Col, L4).

/**
 * matrix_row_reverse(+M, ?R).
 *   Reverse the order of M's rows.
 */
matrix_row_reverse(M, R) :- reverse(M, R).

/**
 * matrix_col_reverse(+M, ?R).
 *   Reverse the order of M's columns.
 */
matrix_col_reverse(M, R) :- map(M, reverse, R).

/**
 * matrix_reverse(+M, ?R).
 *   R is the reverse matrix of M (rows in reverse order, columns in reverse order,
 *   as if by a rotation of 180º).
 */
matrix_reverse(M, R) :- map(M, reverse, T), reverse(T, R).

/**
 * matrix_transpose(?M, ?T).
 *   T is the transpose matrix of M.
 */
matrix_transpose(M, T) :- transpose(M, T).

/**
 * matrix_left_diagonals(+M, ?Ds).
 *   Gets a list of left diagonals of M.
 */
matrix_left_diagonals(M, Ds) :- matrix_size(M, R, C),
                                I is 1 - C, J is R - 1,
                                numlist(I, J, RangeList),
                                map(RangeList, matrix_left_diag(M), Ds).

/**
 * matrix_right_diagonals(+M, ?Ds).
 *   Gets a list of right diagonals of M.
 */
matrix_right_diagonals(M, Ds) :- matrix_size(M, R, C),
                                 I is 1 - C,
                                 J is R - 1,
                                 numlist(I, J, RangeList),
                                 map(RangeList, matrix_right_diag(M), Ds).

/**
 * matrix_diagonals(+M, ?Ds).
 *   Gets a list of diagonals of M.
 */
matrix_diagonals(M, Ds) :- matrix_left_diagonals(M, Ls),
                           matrix_right_diagonals(M, Rs),
                           join(Ls, Rs, Ds).

/**
 * consecutive_any_row(+M, +E, +N).
 *   Asserts the matrix has N consecutive elements E along any row.
 */
consecutive_any_row(M, E, N) :- l_any_of(M, consecutive, [E, N]).

/**
 * consecutive_any_col(+M, +E, +N).
 *   Asserts the matrix has N consecutive elements E along any column.
 */
consecutive_any_col(M, E, N) :- matrix_transpose(M, T), !,
                                l_any_of(T, consecutive, [E, N]).

/**
 * consecutive_any_diag(+M, +E, +N).
 *   Asserts the matrix has N consecutive elements E along any diagonal.
 */
consecutive_any_diag(M, E, N) :- matrix_diagonals(M, Ds), !,
                                 l_any_of(Ds, consecutive, [E, N]).

/**
 * consecutive_matrix(+M, +E, +N).
 *   Asserts the matrix has N consecutive elements E along any row, column or diagonal.
 */
consecutive_matrix(M, E, N) :- consecutive_any_row(M, E, N);
                               consecutive_any_col(M, E, N);
                               consecutive_any_diag(M, E, N).

/**
 * sublist_any_row(+M, +S).
 *   Asserts the matrix has S somewhere along any row.
 */
sublist_any_row(M, S) :- is_list(S), any_of(M, sublist(S)), !.

/**
 * sublist_any_col(+M, +S).
 *   Asserts the matrix has S somewhere along any column.
 */
sublist_any_col(M, S) :- is_list(S),
                         matrix_transpose(M, T), !,
                         any_of(T, sublist(S)), !.

/**
 * sublist_any_diag(+M, +E, +N).
 *   Asserts the matrix has N consecutive elements E along any diagonal.
 */
sublist_any_diag(M, S) :- is_list(S),
                          matrix_diagonals(M, Ds), !,
                          any_of(Ds, sublist(S)), !.

/**
 * sublist_matrix(+M, +E, +N).
 *   Asserts the matrix has N consecutive elements E along any row, column or diagonal.
 */
sublist_matrix(M, S) :- sublist_any_row(M, S);
                        sublist_any_col(M, S);
                        sublist_any_diag(M, S).

/**
 * matrix_min(+M, ?Min).
 *   Using < to compare elements of M, bind Min to the minimum element.
 */
matrix_min(M, Min) :- map(M, list_min, RowMins), list_min(RowMins, Min).

/**
 * matrix_max(+M, ?Max).
 *   Using < to compare elements of M, bind Max to the maximum element.
 */
matrix_max(M, Max) :- map(M, list_max, RowMaxs), list_max(RowMaxs, Max).

/**
 * matrix_index(+M, +E, ?[Row, Col]).
 *   Finds position of the first occurrence of an item E in the matrix M.
 *   Fails if no such item exists.
 */
matrix_index(M, E, [Row, Col]) :- a_index_suchthat(M, contains, E, Row),
                                  matrix_row(M, Row, ListRow), !,
                                  index(ListRow, E, Col).

/**
 * matrix_last_index(+M, +E, ?[Row, Col]).
 *   Finds position of the last occurrence of an item E in the matrix M.
 *   Fails if no such item exists.
 */
matrix_last_index(M, E, [Row, Col]) :- a_last_index_suchthat(M, contains, E, Row),
                                       matrix_row(M, Row, ListRow), !,
                                       last_index(ListRow, E, Col).

/**
 * matrix_indices(+M, +E, ?[Row, Col]).
 *   Finds the indices I of the occurrences of an item E in the list L.
 *   Provides indices of such occurrences.
 */
matrix_indices(M, E, [Row, Col]) :- a_indices_suchthat(M, contains, E, Row),
                                    matrix_row(M, Row, ListRow),
                                    indices(ListRow, E, Col).

















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

test_oldmatrix :- test_all(oldmatrix, [
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
