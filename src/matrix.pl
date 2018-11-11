/**
 * matrix_size(+M, ?R, ?C).
 *   The matrix M is made up of R rows and C columns.
 *   M is a list of lists, all of the same length,
 *   otherwise this predicate fails.
 */
matrix_size(M, R, C) :- length(M, R), a_all_of(M, length, C).

/**
 * is_matrix(+M).
 *   Asserts M is a rectangular matrix.
 */
is_matrix([]).
is_matrix(M) :- matrix_size(M, _, _).

/**
 * matrix_get(+M, +R, +C, -E).
 *   E is the element at position (R,C) in matrix M.
 */
matrix_get(M, R, C, E) :- list_get(M, R, L), !, list_get(L, C, E).

/**
 * matrix_set(+M, +R, +C, +E, -N).
 *   Sets E at position (R,C) on matrix M, with result N.
 */
matrix_set(M, R, C, E, N) :- list_get(M, R, ListRow),
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
 * matrix_transpose(+M, ?T).
 *   T is the transpose matrix of M.
 */
matrix_transpose([], []).
matrix_transpose(M, T) :- matrix_col(M, 1, Col0),
                          matrix_slice(M, 1, 2, N),
                          matrix_transpose(N, NT),
                          push_front(NT, Col0, T).

/**
 * matrix_left_diagonals(+M, ?Ds).
 *   Gets a list of left diagonals of M.
 */
matrix_left_diagonals(M, Ds) :- matrix_size(M, R, C),
                                I is 1 - C, J is R - 1,
                                iota(I, J, IList),
                                map(IList, matrix_left_diag(M), Ds).

/**
 * matrix_right_diagonals(+M, ?Ds).
 *   Gets a list of right diagonals of M.
 */
matrix_right_diagonals(M, Ds) :- matrix_size(M, R, C),
                                 I is 1 - C,
                                 J is R - 1,
                                 iota(I, J, IList),
                                 map(IList, matrix_right_diag(M), Ds).

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
