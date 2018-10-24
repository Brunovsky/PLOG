/**
 * apply(F, L).
 *   Call goal (function) F with arguments L.
 */
apply(F, []) :- call(F). 
apply(F, [A]) :- call(F, A).
apply(F, [A, B]) :- call(F, A, B).
apply(F, [A, B, C]) :- call(F, A, B, C).
apply(F, [A, B, C, D]) :- call(F, A, B, C, D).
apply(F, [A, B, C, D, E]) :- call(F, A, B, C, D, E).

/**
 * loop_increasing(F, I, E).
 *   Call F(N) for N in the range [I, E], with N increasing.
 */
loop_increasing(F, E, E) :- call(F, E).
loop_increasing(F, I, E) :- I < E, call(F, I), Ir is I + 1, loop_increasing(F, Ir, E).

/**
 * loop_decreasing(F, I, E).
 *   Call F(N) for N in the range [I, E], with N decreasing.
 */
loop_decreasing(F, I, I) :- call(F, I).
loop_decreasing(F, I, E) :- I < E, call(F, E), Er is E - 1, loop_decreasing(F, I, Er).
