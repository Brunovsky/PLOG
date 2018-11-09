/**
 * matrix_size(M, R, C).
 *   The matrix M is made up of R rows and C columns.
 *   M is a list of lists, all of the same length,
 *   otherwise this predicate fails.
 */
matrix_size(M, R, C) :- length(M, R), a_all_of(M, length, C).

/**
 * matrix_get(M, R, C, E).
 *   E is the element on row R and column C of matrix M.
 */
matrix_get(M, R, C, E) :- list_get(M, R, L), list_get(L, C, E).

/**
 * matrix_row(M, R, L).
 *   L is the row R of matrix M.
 */
matrix_row(M, R, L) :- list_get(M, R, L).

/**
 * matrix_col(M, C, L).
 *   L is the column C of matrix M.
 */
matrix_col(M, C, L) :- l_map(M, list_get, [C], L).

/**
 * matrix_slice(M, [RowBegin, RowEnd], [ColBegin, ColEnd], N).
 *   Extract a submatrix from M, starting at row RowBegin (inclusive)
 *   and ending at row RowEnd (exclusive), idem for columns.
 */
matrix_slice(M, Row, [ColBegin, ColEnd], N) :-
    integer(Row),
    matrix_size(M, R, C),
    matrix_slice(M, [Row, R], [ColBegin, ColEnd], N).
matrix_slice(M, [RowBegin, RowEnd], Col, N) :-
    integer(Col),
    matrix_size(M, R, C),
    matrix_slice(M, [RowBegin, RowEnd], [Col, C], N).
matrix_slice(M, Row, Col, N) :-
    integer(Row), integer(Col),
    matrix_size(M, R, C),
    matrix_slice(M, [Row, R], [Col, C], N).
matrix_slice(M, [RowBegin, RowEnd], [ColBegin, ColEnd], N) :-
    range(M, RowBegin, RowEnd, M1),
    l_map(M1, range, [ColBegin, ColEnd], M2),
    clear_empty_list(M2, N). % [[],[],[]] --> []

/**
 * matrix_main_diag(M, L).
 *   Extracts the main diagonal from matrix M.
 */
matrix_main_diag([], []).
matrix_main_diag(M, L) :- matrix_get(M, 0, 0, E),
                          matrix_slice(M, 1, 1, N),
                          matrix_main_diag(N, T),
                          push_front(T, E, L).

/**
 * matrix_left_diagonal(M, I, D).
 *   Extracts the main diagonal D from a submatrix of M
 *   starting at row I (or column -I if I is negative).
 *   The elements of D are ordered by column.
 *   
 * matrix_right_diagonal(M, I, D).
 *   Extracts the diagonal perpendicular to the main diagonal
 *   from a submatrix of M starting at row I or column -I.
 *   The elements of D are ordered by column.
 */
matrix_left_diag(M, I, D) :-
    I < 0, J is -I,
    matrix_slice(M, J, 0, N),
    matrix_main_diag(N, D);
    I >= 0,
    matrix_slice(M, 0, I, N),
    matrix_main_diag(N, D).

matrix_right_diag(M, I, D) :-
    J is -I,
    matrix_row_reverse(M, R),
    matrix_left_diag(R, J, D).

/**
 * matrix_row_reverse(M, R).
 *   Reverse the order of M's rows.
 */
matrix_row_reverse(M, R) :- reverse(M, T).

/**
 * matrix_col_reverse(M, R).
 *   Reverse the order of M's columns.
 */
matrix_col_reverse(M, R) :- map(M, reverse, T).

/**
 * matrix_reverse(M, R).
 *   R is the reverse matrix of M (rows in reverse order, columns in reverse order,
 *   as if by a rotation of 180º).
 */
matrix_reverse(M, R) :- map(M, reverse, T), reverse(T, R).

/**
 * matrix_transpose(M, T).
 *   T is the transpose matrix of M.
 */
matrix_transpose([], []).
matrix_transpose(M, T) :- matrix_col(M, 0, C0),
                          matrix_slice(M, 0, 1, N),
                          matrix_transpose(N, NT),
                          push_front(NT, C0, T).
