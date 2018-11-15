/**
 * Lists Library Extension
 *
 * Some predicates here are tested in test/lists.pl
 */

/**
 * Get element at index N: nth0/3, nth1/3
 *   nth0(+N, +List, -Elem), !.
 *   nth1(+N, +List, -Elem), !.
 *   
 * Get first index N (only) of Elem in List: nth0/3, nth1/3
 *   nth0(-N, +List, +Elem), !.
 *   nth1(-N, +List, +Elem), !.
 * 
 * Get last index N (only) of Elem in List: nth0/3, nth1/3, setof/3
 *   setof(-X, nth0(-X, +List, +Elem), Bag), !, last(Bag, -N), !.
 *   setof(-X, nth1(-X, +List, +Elem), Bag), !, last(Bag, -N), !.
 *
 * Get all indices N of Elem in List: nth0/3, nth1/3
 *   nth0(-N, +List, +Elem).
 *   nth1(-N, +List, +Elem).
 *   
 * Replace element X in List with Elem, getting NewList: selectchk/4
 *   selectchk(+X, +List, +Elem, -NewList).
 *
 * Set element Elem at index N in List, getting NewList: selectchknth0/5, selectchknth1/5
 *   selectchknth0(_, +List, +Elem, -NewList, +N).
 *   selectchknth1(_, +List, +Elem, -NewList, +N).
 *
 * Remove element X in List, getting NewList: selectchk/3
 *   selectchk(+X, +List, -NewList).
 *   delete(+List, +X, 1, -NewList).
 *
 * Remove all elements X in List, getting NewList: selectchk/3
 *   delete(+List, +X, -NewList).
 *
 * Remove element at index N in List, getting NewList: selectchknth0/4, selectchknth1/4
 *   nth0(N, +List, _, -NewList).
 *   nth1(N, +List, _, -NewList).
 */

/**
 * selectnth0/5, selectnth1/5.
 * selectnth0(?X, ?Xlist, ?Y, ?Ylist, ?N).
 * selectnth1(?X, ?Xlist, ?Y, ?Ylist, ?N).
 *   Like select/4, but gets the index as well.
 */
selectnth0(X, [X|T], Y, [Y|T], 0).
selectnth0(X, [A,X|T], Y, [A,Y|T], 1).
selectnth0(X, [A,B,X|T], Y, [A,B,Y|T], 2).
selectnth0(X, [A,B,C|Xs], Y, [A,B,C|Ys], N) :- selectnth0(X, Xs, Y, Ys, M), N is M + 3.

selectnth1(X, [X|T], Y, [Y|T], 1).
selectnth1(X, [A,X|T], Y, [A,Y|T], 2).
selectnth1(X, [A,B,X|T], Y, [A,B,Y|T], 3).
selectnth1(X, [A,B,C|Xs], Y, [A,B,C|Ys], N) :- selectnth1(X, Xs, Y, Ys, M), N is M + 3.

/**
 * selectchknth0/5, selectchknth1/5.
 * selectchknth0(?X, ?Xlist, ?Y, ?Ylist, ?N).
 * selectchknth1(?X, ?Xlist, ?Y, ?Ylist, ?N).
 *   Like selectchk/4, but gets the index as well.
 */
selectchknth0(X, Xlist, Y, Ylist, N) :- selectnth0(X, Xlist, Y, Ylist, N), !.

selectchknth1(X, Xlist, Y, Ylist, N) :- selectnth1(X, Xlist, Y, Ylist, N), !.

/**
 * segment/3, proper_segment/3
 * segment(?List, ?Segment, ?N).
 * proper_segment(?List, ?Segment, ?N).
 *   Like segment/2 and proper_segment/2, but binding the segment length.
 */
segment(List, Segment, N) :- length(Segment, N), segment(List, Segment).

proper_segment(List, Segment, N) :- length(Segment, N), proper_segment(List, Segment).

/**
 * fill_n/3
 * fill_n(?N, ?E, ?L).
 *   L is a list with N elements E.
 */
fill_n(N, E, L) :- fill_n_aux(N, E, L), !.
fill_n_aux(0, _, []).
fill_n_aux(N, E, [E|T]) :- fill_n_aux(M, E, T), N is M + 1.

/**
 * range/3
 * range(+List, ?Segment, ?[I, J]).
 *   Segment is the segment between indices I (inclusive) and J (inclusive) in List.
 */
% range(+List, ?Segment, [+I,+J])
range(List, Segment, [I,J]) :- proper_length(List, ListLength),
                               integer(I), integer(J), !,
                               I =< J,
                               Before is I - 1,
                               Length is J - Before,
                               After is ListLength - J,
                               sublist(List, Segment, Before, Length, After).

% range(+List, ?Segment, [+I,-J])
range(List, Segment, [I,J]) :- is_list(List),
                               integer(I), !,
                               Before is I - 1,
                               sublist(List, Segment, Before, Length, _),
                               J is Before + Length.

% range(+List, ?Segment, [-I,+J])
range(List, Segment, [I,J]) :- proper_length(List, ListLength),
                               integer(J), !,
                               After is ListLength - J,
                               sublist(List, Segment, Before, _, After),
                               I is Before + 1.

% range(+List, ?Segment, [-I,-J]).
range(List, Segment, [I,J]) :- is_list(List), !,
                               sublist(List, Segment, Before, Length, _),
                               I is Before + 1,
                               J is Before + Length.

% range(+List, ?Segment, +I).
range(List, Segment, I) :- proper_length(List, ListLength),
                           integer(I), !,
                           range(List, Segment, [I,ListLength]).

/**
 * remove/3
 * remove(+I, +List, -NewList).
 *   Remove I from List, getting NewList.
 */
remove(I, [], []) :- I >= 0.
remove(1, [_|List], List) :- !.
remove(I, [H|List], [H|New]) :- I1 is I - 1, remove(I1, List, New).

/**
 * rangeremove/3
 * rangeremove(+[I,J], +List, -NewList).
 *   Deletes range [I,J] from List, resulting in NewList.
 */
rangeremove([1,0], List, List) :- !.
rangeremove([1,J], [_|Tail], New) :-
    J1 is J - 1,
    rangeremove([1,J1], Tail, New).
rangeremove([I,J], [H|Tail], [H|New]) :-
    I1 is I - 1,
    J1 is J - 1,
    rangeremove([I1,J1], Tail, New).

/**
 * rangeselect/4
 * rangeselect(?XSegment, ?Xlist, ?YSegment, ?Ylist).
 *   True when Xlist and Ylist are the same, except for a range [I,J]
 *   where they are XSegment and YSegment respectively.
 */
rangeselect([], List, [], List) :- is_list(List).
rangeselect([X|Xs], [X|Xlist], [Y|Ys], [Y|Ylist]) :- rangeselect(Xs, Xlist, Ys, Ylist).
rangeselect(Xs, [H|Xlist], Ys, [H|Ylist]) :- rangeselect(Xs, Xlist, Ys, Ylist).

/**
 * rangeselectchk/4
 * rangeselectchk(?XSegment, +Xlist, ?YSegment, +Ylist).
 *   Is to rangeselect/4 what selectchk/4 is to select/4.
 */
rangeselectchk([], List, [], List) :- is_list(List), !.
rangeselectchk([X|Xs], [X|Xlist], [Y|Ys], [Y|Ylist]) :- rangeselectchk(Xs, Xlist, Ys, Ylist), !.
rangeselectchk(Xs, [H|Xlist], Ys, [H|Ylist]) :- rangeselectchk(Xs, Xlist, Ys, Ylist), !.

/**
 * rangeselectnth0/5
 * rangeselectnth0(?Xsegment, ?Xlist, ?YSegment, ?Ylist, ?[I,J]).
 *   True when Xlist and Ylist are the same, except for nonempty range [I,J]
 *   where they are XSegment and YSegment respectively.
 *   May be used to replace a subsegment of Xlist.
 */
rangeselectnth0([X], [X|List], [Y], [Y|List], [0,0]).
rangeselectnth0([X|Xs], [X|Xlist], [Y|Ys], [Y|Ylist], [0,J]) :-
    rangeselectnth0(Xs, Xlist, Ys, Ylist, [0,V]), J is V + 1.
rangeselectnth0([X|Xs], [H|Xlist], [Y|Ys], [H|Ylist], [I,J]) :-
    rangeselectnth0([X|Xs], Xlist, [Y|Ys], Ylist, [U,V]), I is U + 1, J is V + 1.

/**
 * rangeselectnth1/5
 * rangeselectnth1(?Xsegment, ?Xlist, ?YSegment, ?Ylist, ?[I,J]).
 *   True when Xlist and Ylist are the same, except for nonempty range [I,J]
 *   where they are XSegment and YSegment respectively.
 *   May be used to replace a subsegment of Xlist.
 */
rangeselectnth1([X], [X|List], [Y], [Y|List], [1,1]).
rangeselectnth1([X|Xs], [X|Xlist], [Y|Ys], [Y|Ylist], [1,J]) :-
    rangeselectnth1(Xs, Xlist, Ys, Ylist, [1,V]), J is V + 1.
rangeselectnth1([X|Xs], [H|Xlist], [Y|Ys], [H|Ylist], [I,J]) :-
    rangeselectnth1([X|Xs], Xlist, [Y|Ys], Ylist, [U,V]), I is U + 1, J is V + 1.

/**
 * rangeselectchknth0/5
 * rangeselectchknth0(?Xsegment, ?Xlist, ?YSegment, ?Ylist, ?[I,J]).
 *   True when Xlist and Ylist are the same, except for nonempty range [I,J]
 *   where they are XSegment and YSegment respectively.
 *   May be used to replace a subsegment of Xlist.
 */
rangeselectchknth0([X], [X|List], [Y], [Y|List], [0,0]) :- !.
rangeselectchknth0([X|Xs], [X|Xlist], [Y|Ys], [Y|Ylist], [0,J]) :-
    rangeselectchknth0(Xs, Xlist, Ys, Ylist, [0,V]), J is V + 1, !.
rangeselectchknth0([X|Xs], [H|Xlist], [Y|Ys], [H|Ylist], [I,J]) :-
    rangeselectchknth0([X|Xs], Xlist, [Y|Ys], Ylist, [U,V]), I is U + 1, J is V + 1, !.

/**
 * rangeselectchknth1/5
 * rangeselectchknth1(?Xsegment, ?Xlist, ?YSegment, ?Ylist, ?[I,J]).
 *   True when Xlist and Ylist are the same, except for nonempty range [I,J]
 *   where they are XSegment and YSegment respectively.
 *   May be used to replace a subsegment of Xlist.
 */
rangeselectchknth1([X], [X|List], [Y], [Y|List], [1,1]) :- !.
rangeselectchknth1([X|Xs], [X|Xlist], [Y|Ys], [Y|Ylist], [1,J]) :-
    rangeselectchknth1(Xs, Xlist, Ys, Ylist, [1,V]), J is V + 1, !.
rangeselectchknth1([X|Xs], [H|Xlist], [Y|Ys], [H|Ylist], [I,J]) :-
    rangeselectchknth1([X|Xs], Xlist, [Y|Ys], Ylist, [U,V]), I is U + 1, J is V + 1, !.

/**
 * consecutive(+List, +Elem, +N).
 *   Asserts once that List has N consecutive Elem.
 */
consecutive(List, Elem, N) :- fill_n(N, Elem, EList), !, segment(List, EList, N), !.

/**
 * map/3
 * map(:P, ?Xs, ?Ys).
 *   Succeeds when P(X, Y) for each corresponding X in Xs and Y in Ys.
 *   Exactly like maplist/3. One of Xs, Ys must be proper.
 */
map(P, Xs, Ys) :-
    (   foreach(X, Xs),
        foreach(Y, Ys),
        param(P)
    do  call(P, X, Y)
    ).

/**
 * map/4
 * map(:P, ?Xs, ?Ys, ?Zs).
 *   Succeeds when P(X, Y, Z) for each corresponding X in Xs, Y in Ys, Z in Zs.
 *   Exactly like maplist/4. One of Xs, Ys, Zs must be proper.
 */
map(P, Xs, Ys, Zs) :-
    (   foreach(X, Xs),
        foreach(Y, Ys),
        foreach(Z, Zs),
        param(P)
    do  call(P, X, Y, Z)
    ).

/**
 * a_map/4
 * a_map(:P, +A, ?Xs, ?Ys).
 *   Succeeds when P(X, Y, A) for each corresponding X in Xs and Y in Ys.
 *   One of Xs, Ys must be proper.
 */
a_map(P, A, Xs, Ys) :-
    (   foreach(X, Xs),
        foreach(Y, Ys),
        param(P),
        param(A)
    do  call(P, X, Y, A)
    ).

/**
 * a_map/5
 * a_map(:P, +A, ?Xs, ?Ys, ?Zs).
 *   Succeeds when P(X, Y, Z, A) for each corresponding X in Xs, Y in Ys, Z in Zs.
 *   One of Xs, Ys, Zs must be proper.
 */
a_map(P, A, Xs, Ys, Zs) :-
    (   foreach(X, Xs),
        foreach(Y, Ys),
        foreach(Z, Zs),
        param(P),
        param(A)
    do  call(P, X, Y, Z, A)
    ).

/**
 * b_map/4
 * b_map(:P, +B, ?Xs, ?Ys).
 *   Succeeds when P(X, B, Y) for each corresponding X in Xs and Y in Ys.
 *   One of Xs, Ys must be proper.
 */
b_map(P, B, Xs, Ys) :-
    (   foreach(X, Xs),
        foreach(Y, Ys),
        param(P),
        param(B)
    do  call(P, X, B, Y)
    ).

/**
 * la_map/4
 * la_map(:P, +Args, ?Xs, ?Ys).
 *   Succeeds when P(X, Y, Args...) for each corresponding X in Xs and Y in Ys.
 *   One of Xs, Ys must be proper.
 */
la_map(P, Args, Xs, Ys) :-
    is_list(Args),
    (   foreach(X, Xs),
        foreach(Y, Ys),
        param(P),
        param(Args)
    do  apply(P, [X,Y|Args])
    ).

/**
 * la_map/5
 * la_map(:P, +Args, ?Xs, ?Ys, ?Zs).
 *   Succeeds when P(X, Y, Z, Args...) for each corresponding X in Xs, Y in Ys, Z in Zs.
 *   One of Xs, Ys, Zs must be proper.
 */
la_map(P, Args, Xs, Ys, Zs) :-
    is_list(Args),
    (   foreach(X, Xs),
        foreach(Y, Ys),
        foreach(Z, Zs),
        param(P),
        param(Args)
    do  apply(P, [X,Y,Z|Args])
    ).

/**
 * lb_map/4
 * lb_map(:P, +Args, ?Xs, ?Ys).
 *   Succeeds when P(X, Args..., Y) for each corresponding X in Xs and Y in Ys.
 *   One of Xs, Ys must be proper.
 */
lb_map(P, Args, Xs, Ys) :-
    is_list(Args),
    (   foreach(X, Xs),
        foreach(Y, Ys),
        param(P),
        param(Args)
    do  (append([X|Args], [Y], A), apply(P, A))
    ).

/**
 * flatten(+List, -Flat).
 *   Flattens List into Flat.
 *   Fails if List is not a list of lists.
 */
flatten([], []).
flatten([HList|TList], Flat) :- flatten(TList, Rest),
                                  append(HList, Rest, Flat), !.

/**
 * mixed_flatten(+List, -Flat).
 *   Mix flattens List into Flat.
 *   The non-list elements of L are passed unmodified.
 */
mixed_flatten([], []).
mixed_flatten([HList|TList], Flat) :- is_list(HList),
                                      mixed_flatten(TList, Rest),
                                      append(HList, Rest, Flat), !.
mixed_flatten([H|TList], [H|Rest]) :- mixed_flatten(TList, Rest), !.

/**
 * deep_flatten(+List, -Flat).
 *   Deep flattens List into Flat.
 *   List elements of L are flattened themselves.
 */
deep_flatten([], []).
deep_flatten([HList|TList], Flat) :- is_list(HList),
                                     deep_flatten(HList, HFlat),
                                     deep_flatten(TList, Rest),
                                     append(HFlat, Rest, Flat), !.
deep_flatten([H|TList], [H|Rest]) :- deep_flatten(TList, Rest), !.

/**
 * clear_empty_list(+List, -Clear).
 *   Removes from List elements like [].
 */
clear_empty_list(List, Clear) :- delete(List, [], Clear).

/**
 * include_each/3
 * include_each(:P, +Xs, ?SubList).
 *   Filter Xs, including only elements X that verify P(X) into SubList.
 */
include_each(P, Xs, SubList) :-
    (   foreach(X, Xs),
        fromto(SubList, S0, S, []),
        param(P)
    do  (call(P, X) -> S0 = [X|S]; S0 = S)
    ).

/**
 * a_include_each/4
 * a_include_each(:P, +A, +Xs, ?SubList).
 *   Filter Xs, including only elements X that verify P(X, A) into SubList.
 */
a_include_each(P, A, Xs, SubList) :-
    (   foreach(X, Xs),
        fromto(SubList, S0, S, []),
        param(P),
        param(A)
    do  (call(P, X, A) -> S0 = [X|S]; S0 = S)
    ).

/**
 * l_include_each/4
 * l_include_each(:P, +Args, +Xs, ?SubList).
 *   Filter Xs, including only elements X that verify P(X, Args...) into SubList.
 */
l_include_each(P, Args, Xs, SubList) :-
    is_list(Args),
    (   foreach(X, Xs),
        fromto(SubList, S0, S, []),
        param(P),
        param(Args)
    do  (apply(P, [X|Args]) -> S0 = [X|S]; S0 = S)
    ).

/**
 * exclude_each/3
 * exclude_each(:P, +Xs, ?SubList).
 *   Filter Xs, including only elements X that fail P(X) into SubList.
 */
exclude_each(P, Xs, SubList) :-
    (   foreach(X, Xs),
        fromto(SubList, S0, S, []),
        param(P)
    do  (call(P, X) -> S0 = S; S0 = [X|S])
    ).

/**
 * a_exclude_each/4
 * a_exclude_each(:P, +A, +Xs, ?SubList).
 *   Filter Xs, including only elements X that fail P(X, A) into SubList.
 */
a_exclude_each(P, A, Xs, SubList) :-
    (   foreach(X, Xs),
        fromto(SubList, S0, S, []),
        param(P),
        param(A)
    do  (call(P, X, A) -> S0 = S; S0 = [X|S])
    ).

/**
 * l_exclude_each/4
 * l_exclude_each(:P, +Args, +Xs, ?SubList).
 *   Filter Xs, including only elements X that fail P(X, Args...) into SubList.
 */
l_exclude_each(P, Args, Xs, SubList) :-
    is_list(Args),
    (   foreach(X, Xs),
        fromto(SubList, S0, S, []),
        param(P),
        param(Args)
    do  (apply(P, [X|Args]) -> S0 = S; S0 = [X|S])
    ).

/**
 * all_of/2
 * all_of(:P, +Xs).
 *   All the elements X of Xs verify F(X).
 *   Exactly like maplist/2.
 */
all_of(P, Xs) :-
    (   foreach(X, Xs),
        param(P)
    do  call(P, X)
    ), !.

/**
 * a_all_of/3
 * a_all_of(:P, +A, +Xs).
 *   All the elements X of Xs verify F(X, A).
 */
a_all_of(P, A, Xs) :-
    (   foreach(X, Xs),
        param(P),
        param(A)
    do  call(P, X, A)
    ), !.

/**
 * l_all_of/3
 * l_all_of(:P, +Args, +Xs).
 *   All the elements X of Xs verify F(X, Args...).
 */
l_all_of(P, Args, Xs) :-
    (   foreach(X, Xs),
        param(P),
        param(Args)
    do  apply(P, [X|Args])
    ), !.

/**
 * any_of/2
 * any_of(:P, +Xs).
 *   Some element X of Xs verifies P(X).
 *   Exactly like somechk/2.
 */
any_of(P, [X|_]) :- call(P, X), !.
any_of(P, [_|T]) :- any_of(P, T).

/**
 * a_any_of/3
 * a_any_of(:P, +A, +Xs).
 *   Some element X of Xs verifies P(X, A).
 */
a_any_of(P, A, [X|_]) :- call(P, X, A), !.
a_any_of(P, A, [_|T]) :- a_any_of(P, A, T).

/**
 * l_any_of/3
 * l_any_of(:P, +Args, +Xs).
 *   Some element X of Xs verifies P(X, Args...).
 */
l_any_of(P, Args, [X|_]) :- apply(P, [X|Args]), !.
l_any_of(P, Args, [_|T]) :- l_any_of(P, Args, T).

/**
 * none_of/2
 * none_of(:P, +Xs).
 *   none element X of Xs verifies P(X).
 */
none_of(P, X) :- \+ any_of(P, X).

/**
 * a_none_of/3
 * a_none_of(:P, +A, +Xs).
 *   none element X of Xs verifies P(X, A).
 */
a_none_of(P, A, X) :- \+ a_any_of(P, A, X).

/**
 * l_none_of/3
 * l_none_of(:P, +Args, +Xs).
 *   none element X of Xs verifies P(X, Args...).
 */
l_none_of(P, Args, X) :- \+ l_any_of(P, Args, X).

/**
 * contains(+List, +X).
 *   List contains X.
 *   Matches only once.
 */
contains(List, X) :- memberchk(X, List).

/**
 * contains_all(+List, +Set).
 *   List contains all elements of list Set.
 *   Matches only once.
 */
contains_all(List, Set) :- all_of(Set, contains(List)), !.

/**
 * contains_any(+List, +Set).
 *   List contains at least one element of list Set.
 *   Matches only once.
 */
contains_any(List, Set) :- any_of(Set, contains(List)), !.

/**
 * contains_none(+List, +Set).
 *   List contains no elements from list Set.
 *   Matches only once.
 */
contains_none(List, Set) :- none_of(Set, contains(List)), !.

/**
 * foreach(:P, +List).
 *   Call P(H) for each element H of List, irrespective of success.
 */
foreach(_, []).
foreach(P, [H|T]) :- (call(P, H) -> true; true), foreach(P, T).

/**
 * a_foreach(:P, +List, +A).
 *   Call P(H, A) for each element H of List, irrespective of success.
 */
a_foreach(_, [], _).
a_foreach(P, [H|T], A) :- (apply(P, [H|A]) -> true; true), a_foreach(P, T, A).

/**
 * la_foreach(:P, +List, +Args).
 *   Call P(H, Args...) for each element H of List, irrespective of success.
 */
l_foreach(_, [], _).
l_foreach(P, [H|T], Args) :- (apply(P, [H|Args]) -> true; true), l_foreach(P, T, Args).

/**
 * foreach_increasing(+L, :P, +N).
 *   Call P(H, N) for each element H of list L, with N increasing for each element.
 */
foreach_increasing([], _, _).
foreach_increasing([H | T], P, N) :-
    call(P, H, N), M is N + 1,
    foreach_increasing(T, P, M).

/**
 * la_foreach_increasing(+L, :P, +Args, +N).
 *   Call P(H, N, Args...) for each element H of list L, with N increasing for each element.
 */
la_foreach_increasing([], _, _, _).
la_foreach_increasing([H | T], P, Args, N) :-
    apply(P, [H, N | Args]),
    M is N + 1,
    la_foreach_increasing(T, P, Args, M).

/**
 * lb_foreach_increasing(+L, :P, +Args, +N).
 *   Call P(H, Args..., N) for each element H of list L, with N increasing for each element.
 */
lb_foreach_increasing([], _, _, _).
lb_foreach_increasing([H | T], P, Args, N) :-
    last(Args, N, B),
    apply(P, [H | B]),
    M is N + 1,
    lb_foreach_increasing(T, P, Args, M).

/**
 * foreach_decreasing(+L, :P, +N).
 *   Call P(H, N) for each element H of list L, with N decreasing for each element.
 */
foreach_decreasing([], _, _).
foreach_decreasing([H | T], P, N) :-
    call(P, H, N), M is N + 1,
    foreach_decreasing(T, P, M).

/**
 * la_foreach_decreasing(+L, :P, +Args, +N).
 *   Call P(H, N, Args...) for each element H of list L, with N decreasing for each element.
 */
la_foreach_decreasing([], _, _, _).
la_foreach_decreasing([H | T], P, Args, N) :-
    apply(P, [H, N | Args]),
    M is N - 1,
    la_foreach_decreasing(T, P, Args, M).

/**
 * lb_foreach_decreasing(+L, :P, +Args, +N).
 *   Call P(H, Args..., N) for each element H of list L, with N decreasing for each element.
 */
lb_foreach_decreasing([], _, _, _).
lb_foreach_decreasing([H | T], P, Args, N) :-
    last(Args, N, B),
    apply(P, [H | B]),
    M is N - 1,
    lb_foreach_decreasing(T, P, Args, M).

/**
 * index(+List, +Elem, ?I).
 *   Finds the index I of the first occurrence of Elem in List.
 *   Fails if no such Elem exists.
 */
index(List, Elem, I) :-
    is_list(List),
    setof(X, nth1(X, List, Elem), Bag), !,
    head(Bag, I), !.

/**
 * last_index(+List, +Elem, ?I).
 *   Finds the index I of the last occurrence of Elem in the List.
 *   Fails if no such Elem exists.
 */
last_index(List, Elem, I) :-
    is_list(List),
    setof(X, nth1(X, List, Elem), Bag), !,
    last(Bag, I), !.

/**
 * indices(+List, +Elem, ?I).
 *   Finds the indices I of the occurrences of Elem in the List.
 *   Provides indices of such occurrences.
 */
indices(List, Elem, I) :-
    is_list(List),
    nth1(I, List, Elem). % no cut

/**
 * index_suchthat(:P, +List, ?I).
 *   Finds the first index I such that P(H) holds for that index.
 */
index_suchthat(P, [H|_], 1) :- call(P, H), !.
index_suchthat(P, [H|T], I) :- \+ call(P, H),
                               index_suchthat(P, T, J), I is J + 1, !.

/**
 * a_index_suchthat(:P, +List, +A, ?I).
 *   Finds the first index I such that P(H, A) holds for that index.
 */
a_index_suchthat(P, [H|_], A, 1) :- call(P, H, A), !.
a_index_suchthat(P, [H|T], A, I) :- \+ call(P, H, A),
                                    a_index_suchthat(P, T, A, J),
                                    I is J + 1, !.

/**
 * l_index_suchthat(:P, +List, +Args, ?I).
 *   Finds the first index I such that P(H, Args...) holds for that index.
 */
l_index_suchthat(P, [H|_], Args, 1) :- apply(P, [H|Args]), !.
l_index_suchthat(P, [H|T], Args, I) :- \+ apply(P, [H|Args]),
                                       l_index_suchthat(P, T, Args, J),
                                       I is J + 1, !.

/**
 * last_index_suchthat(:P, +List, ?I).
 *   Finds the last index I such that P(H) holds for that index.
 */
last_index_suchthat(P, [H], 1) :- call(P, H), !.
last_index_suchthat(P, [H|T], I) :- call(P, H),
                                    last_index_suchthat(P, T, J),
                                    I is J + 1, !.
last_index_suchthat(P, [H|_], 1) :- call(P, H), !.
last_index_suchthat(P, [_|T], I) :- last_index_suchthat(P, T, J),
                                    I is J + 1, !.

/**
 * last_index_suchthat(:P, +List, A, ?I).
 *   Finds the last index I such that P(H, A) holds for that index.
 */
a_last_index_suchthat(P, [H], A, 1) :- call(P, H, A), !.
a_last_index_suchthat(P, [H|T], A, I) :- call(P, H, A),
                                         a_last_index_suchthat(P, T, A, J),
                                         I is J + 1, !.
a_last_index_suchthat(P, [H|_], A, 1) :- call(P, H, A), !.
a_last_index_suchthat(P, [_|T], A, I) :- a_last_index_suchthat(P, T, A, J),
                                         I is J + 1, !.

/**
 * last_index_suchthat(:P, +List, Args, ?I).
 *   Finds the last index I such that P(H, Args...) holds for that index.
 */
l_last_index_suchthat(P, [H], Args, 1) :- apply(P, [H|Args]), !.
l_last_index_suchthat(P, [H|T], Args, I) :- apply(P, [H|Args]),
                                            l_last_index_suchthat(P, T, Args, J),
                                            I is J + 1, !.
l_last_index_suchthat(P, [H|_], Args, 1) :- apply(P, [H|Args]), !.
l_last_index_suchthat(P, [_|T], Args, I) :- l_last_index_suchthat(P, T, Args, J),
                                            I is J + 1, !.

/**
 * indices_suchthat(:P, +List, ?I).
 *   Finds the first index I such that P(H) holds for that index.
 */
indices_suchthat(P, [H|_], 1) :- call(P, H).
indices_suchthat(P, [_|T], I) :- indices_suchthat(P, T, J),
                                 I is J + 1.

/**
 * a_indices_suchthat(:P, +List, +A, ?I).
 *   Finds the first index I such that P(H, A) holds for that index.
 */
a_indices_suchthat(P, [H|_], A, 1) :- call(P, H, A).
a_indices_suchthat(P, [_|T], A, I) :- a_indices_suchthat(P, T, A, J),
                                      I is J + 1.

/**
 * l_indices_suchthat(:P, +List, +Args, ?I).
 *   Finds the first index I such that P(H, Args...) holds for that index.
 */
l_indices_suchthat(P, [H|_], Args, 1) :- apply(P, [H|Args]).
l_indices_suchthat(P, [_|T], Args, I) :- l_indices_suchthat(P, T, Args, J),
                                         I is J + 1.
