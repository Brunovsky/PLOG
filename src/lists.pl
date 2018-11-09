/**
 * is_list(L).
 *   true if L is a list, including [].
 */
is_list([]).
is_list([_ | _]).

/**
 * get(L, N, C).
 *   C is the element at position N in list L (0-indexed).
 */
list_get([H | _], 0, H).
list_get([_ | T], N, C) :- N > 0, M is N - 1, list_get(T, M, C).

/**
 * join(A, B, R).
 *   Concatenating A and B results in R.
 */
join([], [], []).
join([], [H | T], [H | T]).
join([X | A], B, [X | C]) :- join(A, B, C).

/**
 * push_front(L, X, R).
 *   Pushing X into the front of L results in R.
 */
push_front([], X, [X]).
push_front([H | T], X, [X | [H | T]]).

/**
 * push_back(L, X, R).
 *   Pushing X into the back of L results in R.
 */
push_back([], X, [X]).
push_back([H | T], X, [H | R]) :- push_back(T, X, R).

/**
 * pop_front(L, R).
 *   Popping the front element of L results in R (i.e. R is the tail of L).
 */
pop_front([_ | T], T).

/**
 * pop_back(L, R).
 *   Popping the back element of L results in R.
 */
pop_back([_], []).
pop_back([H | T], [H | R]) :- pop_back(T, R).

/**
 * back(L, X).
 *   X is the back (last element) of L.
 */
back([X], X).
back([_ | T], B) :- back(T, B).

/** 
 * head(L, X).
 * front(L, X).
 *   X is the head of L.
 */
front([H | _], H).
head([H | _], H).

/**
 * tail(L, T).
 * shift(L, T).
 *   T is the tail of L.
 */
tail([_ | T], T).
shift([_ | T], T).

/**
 * fill_n(N, E, L).
 *   Fills list L with a total of N elements E.
 */
fill_n(0, _, []).
fill_n(N, E, [E | T]) :- M is N - 1, fill_n(M, E, T).

/**
 * index(L, E, I).
 *   Finds the index I of the first occurence of an item E in the list L.
 *   Fails if no such index exists.
 */
index([E | _], E, 0).
index([_ | T], E, I) :- index(T, E, J), !, I is J + 1.

/**
 * range(L, I, J, R).
 *   Extracts the sublist starting at index I (inclusive) and ending
 *   at index J (exclusive) from L into R.
 */
range(_, 0, 0, []).
range([], I, J, []) :- I =< J.
range([H | T], 0, J, [H | R]) :- J > 0, Jr is J - 1, range(T, 0, Jr, R).
range([_ | T], I, J, R) :- I > 0, Ir is I - 1, J > 0, Jr is J - 1, range(T, Ir, Jr, R).

/**
 * range_n(L, I, N, R).
 *   Extracts the sublist starting at index I (inclusive) with length
 *   N or until the end of the list, from L into R.
 */
range_n(L, I, N, R) :- J is I + N, range(L, I, J, R).

/**
 * reverse(L, R).
 *   R is the list L but in reverse order.
 */
reverse([], []).
reverse([H | T], R) :- reverse(T, Z), push_back(Z, H, R).

/**
 * map(L, F, R).
 *   Calling F once for each element E of L as F(E, Er), so that
 *   R is the list consisting of the resulting Er.
 */
map([], _, []).
map([H | T], F, [Hr | Tr]) :- call(F, H, Hr), map(T, F, Tr).

/**
 * l_map(L, F, Args, R).
 *   Calling F once for each element E of L as F(E, Args..., Er), so that
 *   R is the list consisting of the resulting Er.
 */
l_map([], _, _, []).
l_map([H | T], F, Args, [Hr | Tr]) :- push_back(Args, Hr, Z), apply(F, [H | Z]), l_map(T, F, Args, Tr).

/**
 * flatten(L, R).
 *   Flattens the list L into list R. This fails if L is not a list of lists.
 */
flatten([], []).
flatten([L | T], R) :- flatten(T, T1), join(L, T1, R).

/**
 * mixed_flatten(L, R).
 *   Flattens the list L into list R. The non-list elements of L are passed unmodified.
 */
mixed_flatten([], []).
mixed_flatten([L | T], R) :- is_list(L), mixed_flatten(T, T1), join(L, T1, R).
mixed_flatten([H | T], [H | R]) :- \+ is_list(H), mixed_flatten(T, R).

/**
 * deep_flatten(L, R).
 *   Deep flattens the list L into list R. List elements of list L are flattened themselves.
 */
deep_flatten([], []).
deep_flatten([L | T], R) :- is_list(L), deep_flatten(L, L1), deep_flatten(T, T1), join(L1, T1, R).
deep_flatten([H | T], [H | R]) :- \+ is_list(H), deep_flatten(T, R).

/**
 * include_each(L, F, R).
 *   Filter list L, include only elements H that verify F(H) into list R.
 */
include_each([], _, []).
include_each([H | T], F, [H | R]) :- call(F, H), include_each(T, F, R).
include_each([H | T], F, R) :- \+ call(F, H), include_each(T, F, R).

/**
 * a_include_each(L, F, A, R).
 *   Filter list L, include only elements H that verify F(H, A) into list R.
 */
a_include_each([], _, _, []).
a_include_each([H | T], F, A, [H | R]) :- call(F, H, A), a_include_each(T, F, A, R).
a_include_each([H | T], F, A, R) :- \+ call(F, H, A), a_include_each(T, F, A, R).

/**
 * b_include_each(L, F, B, R).
 *   Filter list L, include only elements H that verify F(B, H) into list R.
 */
b_include_each([], _, _, []).
b_include_each([H | T], F, B, [H | R]) :- call(F, B, H), b_include_each(T, F, B, R).
b_include_each([H | T], F, B, R) :- \+ call(F, B, H), b_include_each(T, F, B, R).

/**
 * l_include_each(L, F, Args, R).
 *   Filter list L, include only elements H that verify F(H, Args...) into list R.
 */
l_include_each([], _, _, []).
l_include_each([H | T], F, Args, [H | R]) :- apply(F, [H | Args]), l_include_each(T, F, Args, R).
l_include_each([H | T], F, Args, R) :- \+ apply(F, [H | Args]), l_include_each(T, F, Args, R).

/**
 * exclude_each(L, F, R).
 *   Filter list L, exclude all elements H that verify F(H) from list R.
 */
exclude_each([], _, []).
exclude_each([H | T], F, [H | R]) :- \+ call(F, H), exclude_each(T, F, R).
exclude_each([H | T], F, R) :- call(F, H), exclude_each(T, F, R).

/**
 * a_exclude_each(L, F, A, R).
 *   Filter list L, exclude all elements H that verify F(H, A) from list R.
 */
a_exclude_each([], _, _, []).
a_exclude_each([H | T], F, A, [H | R]) :- \+ call(F, H, A), a_exclude_each(T, F, A, R).
a_exclude_each([H | T], F, A, R) :- call(F, H, A), a_exclude_each(T, F, A, R).

/**
 * b_exclude_each(L, F, B, R).
 *   Fitler list L, exclude all elements H that verify F(B, H) from list R.
 */
b_exclude_each([], _, _, []).
b_exclude_each([H | T], F, B, [H | R]) :- \+ call(F, B, H), b_exclude_each(T, F, B, R).
b_exclude_each([H | T], F, B, R) :- call(F, B, H), b_exclude_each(T, F, B, R).

/**
 * l_exclude_each(L, F, Args, R).
 *   Filter list L, exclude all elements H that verify F(H, Args...) from list R.
 */
l_exclude_each([], _, _, []).
l_exclude_each([H | T], F, Args, [H | R]) :- \+ apply(F, [H | Args]), l_exclude_each(T, F, Args, R).
l_exclude_each([H | T], F, Args, R) :- apply(F, [H | Args]), l_exclude_each(T, F, Args, R).

/**
 * all_of(L, F).
 *   All the elements H of list L verify F(H).
 */
all_of([], _).
all_of([H | T], F) :- call(F, H), all_of(T, F).

/**
 * a_all_of(L, F, A).
 *   All the elements H of list L verify F(H, A).
 */
a_all_of([], _, _).
a_all_of([H | T], F, A) :- call(F, H, A), a_all_of(T, F, A).

/**
 * b_all_of(L, F, B).
 *   All the elements H of list L verify F(B, H).
 */
b_all_of([], _, _).
b_all_of([H | T], F, B) :- call(F, B, H), b_all_of(T, F, B).

/**
 * l_all_of(L, F, Args).
 *   All the elements H of list L verify F(H, Args...).
 */
l_all_of([], _, _).
l_all_of([H | T], F, Args) :- apply(F, [H | Args]), l_all_of(T, F, Args).

/**
 * any_of(L, F).
 *   At least one element H of list L verifies F(H).
 */
any_of([H | T], F) :- call(F, H); any_of(T, F).

/**
 * a_any_of(L, F, A).
 *   At least one element H of list L verifies F(H, A).
 */
a_any_of([H | T], F, A) :- call(F, H, A); a_any_of(T, F, A).

/**
 * b_any_of(L, F, B).
 *   At least one element H of list L verifies F(B, H).
 */
b_any_of([H | T], F, B) :- call(F, B, H); b_any_of(T, F, B).

/**
 * l_any_of(L, F, Args).
 *   At least one element H of list L verifies F(H, Args...).
 */
l_any_of([H | T], F, Args) :- apply(F, [H | Args]); l_any_of(T, F, Args).

/**
 * none_of(L, F).
 *   No element H of list L verifies F(H).
 */
none_of([], _).
none_of([H | T], F) :- \+ call(F, H), none_of(T, F).

/**
 * a_none_of(L, F, A).
 *   No element H of list L verifies F(H, A).
 */
a_none_of([], _, _).
a_none_of([H | T], F, A) :- \+ call(F, H, A), a_none_of(T, F, A).

/**
 * b_none_of(L, F, B).
 *   No element H of list L verifies F(B, H).
 */
b_none_of([], _, _).
b_none_of([H | T], F, B) :- \+ call(F, B, H), b_none_of(T, F, B).

/**
 * l_none_of(L, F, Args).
 *   No element H of list L verifies F(H, Args...).
 */
l_none_of([], _, _).
l_none_of([H | T], F, Args) :- \+ apply(F, [H | Args]), l_none_of(T, F, Args).

/**
 * count(L, F, N).
 *   Count the elements of L that pass F(H) into N.
 */
count([], _, 0).
count([H | T], F, N) :- call(F, H), count(T, F, M), N is M + 1.
count([H | T], F, N) :- \+ call(F, H), count(T, F, N).

/**
 * a_count(L, F, A, N).
 *   Count the elements of L that pass F(H, A) into N.
 */
a_count([], _, _, 0).
a_count([H | T], F, A, N) :- call(F, H, A), count(T, F, A, M), N is M + 1.
a_count([H | T], F, A, N) :- \+ call(F, H, A), count(T, F, A, N).

/**
 * b_count(L, F, B, N).
 *   Count the elements of L that pass F(B, H) into N.
 */
b_count([], _, _, 0).
b_count([H | T], F, B, N) :- call(F, B, H), count(T, F, B, M), N is M + 1.
b_count([H | T], F, B, N) :- \+ call(F, B, H), count(T, F, B, N).

/**
 * l_count(L, F, Args, N).
 *   Count the elements of L that pass F(H, Args...) into N.
 */
l_count([], _, _, 0).
l_count([H | T], F, Args, N) :- apply(F, [H | Args]), count(T, F, Args, M), N is M + 1.
l_count([H | T], F, Args, N) :- \+ apply(F, [H | Args]), count(T, F, Args, N).

/**
 * contains(L, X).
 *   List L contains X.
 */
contains([H | T], X) :- H = X; contains(T, X).

/**
 * contains_all(L, S).
 *   List L contains all elements of list S.
 */
contains_all(_, []).
contains_all(L, [H | T]) :- contains(L, H), contains_all(L, T).

/**
 * contains_any(L, S).
 *   List L contains at least one element of list S.
 */
contains_any(L, [H | T]) :- contains(L, H); contains_any(L, T).

/**
 * contains_none(L, S).
 *   List L contains no elements from list S.
 */
contains_none(_, []).
contains_none(L, [H | T]) :- \+ contains(L, H), contains_none(L, T).

/**
 * foreach(L, F).
 *   Call F(H) for each element H of list L.
 *   Same as all_of.
 */
foreach([], _).
foreach([H | T], F) :- call(F, H), foreach(T, F).

/**
 * la_foreach(L, F, Args).
 *   Call F(H, Args...) for each element H of list L.
 */
la_foreach([], _, _).
la_foreach([H | T], F, Args) :- apply(F, [H | Args]), la_foreach(T, F, Args).

/**
 * lb_foreach(L, F, Args).
 *   Call F(Args..., H) for each element H of list L.
 */
lb_foreach([], _, _).
lb_foreach([H | T], F, Args) :- push_back(Args, H, B), apply(F, B), lb_foreach(T, F, Args).

/**
 * foreach_increasing(L, F, N).
 *   Call F(H, N) for each element H of list L, with N increasing for each element.
 */
foreach_increasing([], _, _).
foreach_increasing([H | T], F, N) :- call(F, H, N), M is N + 1,
                                     foreach_increasing(T, F, M).

/**
 * la_foreach_increasing(L, F, Args, N).
 *   Call F(H, N, Args...) for each element H of list L, with N increasing for each element.
 */
la_foreach_increasing([], _, _, _).
la_foreach_increasing([H | T], F, Args, N) :- apply(F, [H, N | B]),
                                              M is N + 1,
                                              la_foreach_increasing(T, F, Args, M).

/**
 * lb_foreach_increasing(L, F, Args, N).
 *   Call F(H, Args..., N) for each element H of list L, with N increasing for each element.
 */
lb_foreach_increasing([], _, _, _).
lb_foreach_increasing([H | T], F, Args, N) :- push_back(Args, N, B),
                                              apply(F, [H | B]),
                                              M is N + 1,
                                              lb_foreach_increasing(T, F, Args, M).

/**
 * foreach_decreasing(L, F, N).
 *   Call F(H, N) for each element H of list L, with N decreasing for each element.
 */
foreach_decreasing([], _, _).
foreach_decreasing([H | T], F, N) :- call(F, H, N), M is N + 1,
                                     foreach_decreasing(T, F, M).

/**
 * la_foreach_decreasing(L, F, Args, N).
 *   Call F(H, N, Args...) for each element H of list L, with N decreasing for each element.
 */
la_foreach_decreasing([], _, _, _).
la_foreach_decreasing([H | T], F, Args, N) :- apply(F, [H, N | B]),
                                              M is N - 1,
                                              la_foreach_decreasing(T, F, Args, M).

/**
 * lb_foreach_decreasing(L, F, Args, N).
 *   Call F(H, Args..., N) for each element H of list L, with N decreasing for each element.
 */
lb_foreach_decreasing([], _, _, _).
lb_foreach_decreasing([H | T], F, Args, N) :- push_back(Args, N, B),
                                              apply(F, [H | B]),
                                              M is N - 1,
                                              lb_foreach_decreasing(T, F, Args, M).
