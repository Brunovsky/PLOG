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
 * matrix_get(+M, +R, +C, ?E).
 *   E is the element at position (R,C) in matrix M.
 */
matrix_get(M, R, C, E) :- is_matrix(M), list_get(M, R, L), !, list_get(L, C, E).

/**
 * matrix_set(+M, +R, +C, +E, ?N).
 *   Sets E at position (R,C) on matrix M, with result N.
 */
matrix_set(M, R, C, E, N) :- is_matrix(M), list_get(M, R, L), !, list_set(L, C, E, N).

/**
 * matrix_row(+M, +R, ?L).
 *   L is the row R of matrix M.
 */
matrix_row(M, R, L) :- is_matrix(M), list_get(M, R, L).

/**
 * matrix_col(+M, +C, ?L).
 *   L is the column C of matrix M.
 */
matrix_col(M, C, L) :- is_matrix(M), l_map(M, list_get, [C], L).

/**
 * matrix_slice(+M, +Row, +Col, ?N).
 * matrix_slice(+M, +Row, +[ColBegin, ColEnd], ?N).
 * matrix_slice(+M, [RowBegin, RowEnd], +Col, ?N).
 * matrix_slice(+M, +[RowBegin, RowEnd], +[ColBegin, ColEnd], ?N).
 *   Extract a submatrix from M, starting at row RowBegin (inclusive)
 *   and ending at row RowEnd (exclusive), idem for columns.
 */
matrix_slice(M, Row, Col, N) :-
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

/**
 * matrix_main_diag(+M, ?D).
 *   Extracts the main diagonal from matrix M.
 */
matrix_main_diag([], []).
matrix_main_diag(M, [E | T]) :- is_matrix(M),
                                matrix_get(M, 0, 0, E),
                                matrix_slice(M, 1, 1, N),
                                matrix_main_diag(N, T).

/**
 * matrix_left_diagonal(+M, +I, ?D).
 *   Extracts the main diagonal D from a submatrix of M
 *   starting at row I (or column -I if I is negative).
 *   The elements of D are ordered by row.
 */
matrix_left_diag(M, I, D) :- I < 0, J is -I,
                             matrix_slice(M, 0, J, N),
                             matrix_main_diag(N, D);
                             I >= 0,
                             matrix_slice(M, I, 0, N),
                             matrix_main_diag(N, D).

/**
 * matrix_right_diagonal(+M, +I, ?D).
 *   Extracts the diagonal perpendicular to the main diagonal
 *   from a submatrix of M starting at row I or column -I.
 *   This is the diagonal starting at the top right corner.
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
matrix_right_diag_through(M, Row, Col, D) :- I is Row - Col,
                                             matrix_right_diag(M, I, D).

/**
 * matrix_through(+M, +Row, +Col, ?Ls).
 *   Extracts the row, column, and two diagonals passing through (Row, Col).
 */
matrix_through(M, Row, Col, [L1, L2, L3, L4]) :- matrix_row(M, Row, L1),
                                                 matrix_col(M, Col, L2),
                                                 matrix_left_diag(M, Row, Col, L3),
                                                 matrix_right_diag(M, Row, Col, L4),

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
matrix_transpose(M, T) :- matrix_col(M, 0, Col0),
                          matrix_slice(M, 0, 1, N),
                          matrix_transpose(N, NT),
                          push_front(NT, Col0, T).

/**
 * matrix_left_diagonals(+M, ?Ds).
 *   Gets a list of left diagonals of M.
 */
matrix_left_diagonals(M, Ds) :- matrix_size(M, R, C),
                                I is 1 - C,
                                J is R - 1,
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
consecutive_any_col(M, E, N) :- matrix_transpose(M, T),
                                l_any_of(T, consecutive, [E, N]).

/**
 * consecutive_any_diag(+M, +E, +N).
 *   Asserts the matrix has N consecutive elements E along any diagonal.
 */
consecutive_any_diag(M, E, N) :- matrix_diagonals(M, Ds),
                                 l_any_of(Ds, consecutive, [E, N]).

/**
 * consecutive_matrix(+M, +E, +N).
 *   Asserts the matrix has N consecutive elements E along any row, column or diagonal.
 */
consecutive_matrix(M, E, N) :- consecutive_any_row(M, E, N);
                               consecutive_any_col(M, E, N);
                               consecutive_any_diag(M, E, N).

