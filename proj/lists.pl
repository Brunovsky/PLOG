/**
 * is_list(L).
 *   true if L is a list, including [].
 */
is_list([]).
is_list([_ | _]).

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
 * front(L, X).
 *   X is the head (first element) of L.
 */
front([H | _], H).

/**
 * back(L, X).
 *   X is the back (last element) of L.
 */
back([X], X).
back([_ | T], B) :- back(T, B).

/** 
 * head(L, X).
 *   X is the head of L.
 */
head([H | _], H).

/**
 * tail(L, T).
 *   T is the tail of L.
 */
tail([_ | T], T).

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


include_each([], _, []).
include_each([H | T], F, [H | R]) :- call(F, H), include_each(T, F, R).
include_each([H | T], F, R) :- \+ call(F, H), include_each(T, F, R).

a_include_each([], _, _, []).
a_include_each([H | T], F, A, [H | R]) :- call(F, H, A), a_include_each(T, F, A, R).
a_include_each([H | T], F, A, R) :- \+ call(F, H, A), a_include_each(T, F, A, R).

b_include_each([], _, _, []).
b_include_each([H | T], F, B, [H | R]) :- call(F, B, H), b_include_each(T, F, B, R).
b_include_each([H | T], F, B, R) :- \+ call(F, B, H), b_include_each(T, F, B, R).


exclude_each([], _, []).
exclude_each([H | T], F, [H | R]) :- \+ call(F, H), exclude_each(T, F, R).
exclude_each([H | T], F, R) :- call(F, H), exclude_each(T, F, R).

a_exclude_each([], _, _, []).
a_exclude_each([H | T], F, A, [H | R]) :- \+ call(F, H, A), a_exclude_each(T, F, A, R).
a_exclude_each([H | T], F, A, R) :- call(F, H, A), a_exclude_each(T, F, A, R).

b_exclude_each([], _, _, []).
b_exclude_each([H | T], F, B, [H | R]) :- \+ call(F, B, H), b_exclude_each(T, F, B, R).
b_exclude_each([H | T], F, B, R) :- call(F, B, H), b_exclude_each(T, F, B, R).


all_of([], _).
all_of([H | T], F) :- call(F, H), all_of(T, F).

a_all_of([], _, _).
a_all_of([H | T], F, A) :- call(F, H, A), a_all_of(T, F, A).

b_all_of([], _, _).
b_all_of([H | T], F, B) :- call(F, B, H), b_all_of(T, F, B).


any_of([H | T], F) :- call(F, H); any_of(T, F).

a_any_of([H | T], F, A) :- call(F, H, A); a_any_of(T, F, A).

b_any_of([H | T], F, B) :- call(F, B, H); b_any_of(T, F, B).


none_of([], _).
none_of([H | T], F) :- \+ call(F, H), none_of(T, F).

a_none_of([], _, _).
a_none_of([H | T], F, A) :- \+ call(F, H, A), a_none_of(T, F, A).

b_none_of([], _, _).
b_none_of([H | T], F, B) :- \+ call(F, B, H), b_none_of(T, F, B).


contains([H | T], X) :- H = X; contains(T, X).

contains_all(_, []).
contains_all(L, [H | T]) :- contains(L, H), contains_all(L, T).

contains_any(L, [H | T]) :- contains(L, H); contains_any(L, T).

contains_none(_, []).
contains_none(L, [H | T]) :- \+ contains(L, H), contains_none(L, T).
