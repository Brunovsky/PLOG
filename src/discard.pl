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
