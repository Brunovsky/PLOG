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
 * matrix_left_diagonal(M, I, L).
 * matrix_right_diagonal(M, I, L).
 *   Consider
 *              [ 1   3   4   9 ]
 *              [ 7   2   3   8 ]
 *              [ 8   0   6   1 ]
 *              [ 0   2   4   3 ]
 *   The left diagonal 0 is the main diagonal [1,2,6,3].
 *   The left diagonal -1 is above it, [3,3,1].
 *   The left diagonal 1 is below it, [7,0,4]. And so on.
 *   The right diagonal 0 is [0,0,3,9].
 *   The right diagonal -1 is above it, [8,2,4].
 *   The right diagonal 1 is [2,6,8]. And so on.
 */
matrix_left_diag_below(M, I, L) :- matrix_slice(M, I, 0, N),
                                   matrix_main_diag(N, L).
matrix_left_diag_above(M, I, L) :- matrix_slice(M, 0, I, N),
                                   matrix_main_diag(N, L).
matrix_left_diag(M, I, L) :-
    I < 0, J is -I,
    matrix_slice(M, J, 0, N),
    matrix_main_diag(N, L);
    I >= 0,
    matrix_slice(M, 0, I, N),
    matrix_main_diag(N, L).

matrix_right_diag(M, I, L) :-
    J is -I,
    reverse(M, R),
    matrix_left_diag(R, J, L).

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
