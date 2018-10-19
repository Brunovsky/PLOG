/**
 * check_matrix(M, R, C).
 *   The matrix M is made up of R rows and C columns.
 */
check_matrix(M, R, C) :- length(M, R), a_all_of(M, length, C).

/**
 * matrix_get(M, R, C, E).
 *   E is the element on row R and column C of matrix M.
 */
matrix_get(M, R, C, E) :- list_get(M, R, L), list_get(L, C, E).

/**
 * matrix_reverse(M, R).
 *   R is the reverse matrix of M (rows in reverse order, columns in reverse order,
 *   as if by a rotation of 180º).
 */
matrix_reverse(M, R) :- map(M, reverse, T), reverse(T, R).
