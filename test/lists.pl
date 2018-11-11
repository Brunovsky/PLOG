% list_get(L, N, C).
test_list_get :-
    list_get([0,1,2,3,4], 2, 2),
    list_get([a,b,c,d,e,f], 4, e),
    list_get([a,b], 0, a),
    \+ list_get([], 0, _),
    \+ list_get([a,b,c,d], 4, _).

% list_set(L, N, E, C).
test_list_set :-
    list_set([0,1,2,3,4], 2, a, [0,1,a,3,4]),
    list_set([0,1,2,3,4,5,6], 3, b, [0,1,2,b,4,5,6]),
    list_set([a,b], 0, 1, [1,b]),
    \+ list_set([], 0, _, _),
    \+ list_set([a,b], 2, _, _).

% prefix(J, L).
test_prefix :-
    prefix([1,2,3], [1,2,3,4]),
    prefix([1,2,3], [1,2,3]),
    prefix([X], [X | _]),
    \+ prefix([_ | _], []),
    \+ prefix([1,2,3], [1,2,4]),
    \+ prefix([2,3], [1,2,3]).

% proper_prefix(J, L).
test_proper_prefix :-
    proper_prefix([1,2,3], [1,2,3,4]),
    \+ proper_prefix([1,2,3], [1,2,3]),
    proper_prefix([X], [X | [_ | _]]),
    \+ proper_prefix(_, []),
    \+ proper_prefix([1,2,3], [1,2,4]),
    \+ proper_prefix([1,2,3], [1,2]).

% suffix(J, L).
test_suffix :-
    suffix([2,3,4], [1,2,3,4]),
    suffix([1,2,3], [1,2,3]),
    suffix([X], [_ | X]),
    \+ suffix([_ | _], []),
    \+ suffix([1,2,3], [1,2,4]),
    \+ suffix([1,2], [1,2,3]).

% proper_suffix(J, L).
test_proper_suffix :-
    proper_suffix([2,3,4], [1,2,3,4]),
    \+ proper_suffix([1,2,3], [1,2,3]),
    proper_suffix([X], [_ | X]),
    \+ proper_suffix([_ | _], []),
    \+ proper_suffix([1,2,3], [1,2,4]),
    \+ proper_suffix([1,2], [1,2,3]).

% sublist(J, L).
test_sublist :-
    sublist([5,6,7], [0,1,2,3,4,5,6,7,8,9]),
    sublist([0,1,2,3], [0,1,2,3,4,5,6]),
    sublist([5,6,7,8], [0,1,2,3,4,5,6,7,8]),
    contains([0,1,2,3,4,5,6], X), sublist([X], [0,1,2,3,4,5,6]),
    sublist([], [_ | _]),
    sublist([1,2,3], [1,2,3]),
    \+ sublist([2,3,5], [0,1,2,3,4,5,6]),
    \+ sublist([4,5], [0,1,2,3,4]).

% sublist_n(J, L, N).
test_sublist_n :-
    sublist_n([1,2,3,4], [0,1,2,3,4,5,6], 4),
    sublist_n([1], [0,1,2,3], 1),
    sublist_n([], [_ | _], 0),
    sublist_n([0,1,2,3], [0,1,2,3 | _], 4),
    \+ sublist_n([2,3], [0,1,2,3,4,5], 3),
    \+ sublist_n([2,3], [0,1,2,4,5], 2).

% join(A, B, R).
test_join :-
    join([a,b,c,d], [e,f,g], [a,b,c,d,e,f,g]),
    join([a,a,a], [b,b], [a,a,a,b,b]),
    join([a,a], [], [a,a]),
    join([], [b,b,b], [b,b,b]),
    join([], [], []).

% push_front(L, X, R).
test_push_front :-
    push_front([a,b,c,d], 0, [0,a,b,c,d]),
    push_front([], 0, [0]),
    \+ push_front(_, _, []),
    push_front([0,1,2], 3, [3,0,1,2]).

% push_back(L, X, R).
test_push_back :-
    push_back([a,b,c,d], e, [a,b,c,d,e]),
    push_back([], 0, [0]),
    \+ push_back(_, _, []),
    push_back([0,1,2], 3, [0,1,2,3]).

% pop_front(L, R).
test_pop_front :-
    pop_front([a,b,c], [b,c]),
    pop_front([b], []),
    pop_front([0,1,0], [1,0]),
    \+ pop_front([], _).

% pop_back(L, R).
test_pop_back :-
    pop_back([a,b,c], [a,b]),
    pop_back([b], []),
    pop_back([0,1,0], [0,1]),
    \+ pop_back([], _).

% fill_n(N, E, L).
test_fill_n :-
    fill_n(4, a, [a,a,a,a]),
    fill_n(7, b, [b,b,b,b,b,b,b]),
    fill_n(2, 0, [0,0]),
    fill_n(0, a, []),
    fill_n(3, [a,X], [[a,X],[a,X],[a,X]]),
    \+ fill_n(3, a, [a,a,a,a]),
    \+ fill_n(5, c, [c,c,c]).

% iota(I, J, L).
test_iota :-
    iota(10, 14, [10,11,12,13,14]),
    iota(-3, 7, [-3,-2,-1,0,1,2,3,4,5,6,7]),
    iota(4, 4, [4]),
    \+ iota(4, 3, _),
    iota(9, 14, [9, 10, 11, 12, 13, 14]).

% range(L, I, R).
test_range :-
    range([0,1,2,3,4,5], [2,5], [2,3,4]),
    range([a,b,c,d,e,f,g], [1, 6], [b,c,d,e,f]),
    range([a,b,c,d], [2, 10], [c,d]),
    range([], _, []),
    range([a,b,c], [3,5], []),
    range([0,1,2,3,4,5], 3, [3,4,5]),
    range([a,b,c,d,e,f,g], 4, [e,f,g]).

% range_n(L, I, R).
test_range_n :-
    range_n([0,1,2,3,4,5,6], [2,4], [2,3,4,5]),
    range_n([a,b,c,d,e], [1,5], [b,c,d,e]),
    range_n([a,b,c,d,e], [2,0], []).

% consecutive(L, E, N).
test_consecutive :-
    consecutive([0,1,3,3,2,4,2,3,3,3,7,4], 3, 3),
    \+ consecutive([0,0,3,3,4,9,7,3,4,3,0,3,3,3,3,5], 3, 5),
    consecutive([6,4,2,8,3,4,4,5,8,8,9,9,9,8,1,2,3,5,6,8,8,2,3,9], 9, 3),
    consecutive([3,2,4,4,4,4,2,0], 4, 4),
    \+ consecutive([3,2,4,4,4,2,0], 4, 4),
    \+ consecutive([0,1], a, 5).

% reverse(L, R).
test_reverse :-
    reverse([a,b,c,d], [d,c,b,a]),
    reverse([a,b], [b,a]),
    reverse([], []).

% map(L, F, R).
test_map :-
    map([[a,b,c],[d,e],[f,g,h,i]], length, [3,2,4]),
    map([[a,b,c],[d,e],[f,g,h,i]], back, [c,e,i]),
    map([[a,b,c],[d,e,f]], reverse, [[c,b,a],[f,e,d]]),
    map([], _, []).

% l_map(L, F, Args, R).
test_l_map :-
    l_map([[a,b,c],[d,e],[f,g,h,i]], list_get, [1], [b,e,g]),
    l_map([[a,b,a],[b,b,a,b],[c,c,b,a]], index, [a], [0,2,3]),
    l_map([3,4,2], fill_n, [a], [[a,a,a],[a,a,a,a],[a,a]]),
    l_map([[a,b,c,d],[e,f,g,h]], range_n, [[1,2]], [[b,c],[f,g]]).

% flatten(L, R).
test_flatten :-
    flatten([[a,b,c],[d,e,f]], [a,b,c,d,e,f]),
    \+ flatten([[a,b],c,d], _),
    flatten([[a,b,c],[d,[e,f]]], [a,b,c,d,[e,f]]).

% mixed_flatten(L, R).
test_mixed_flatten :-
    mixed_flatten([[a,b,c],[d,e,f]], [a,b,c,d,e,f]),
    mixed_flatten([[a,b],c,d], [a,b,c,d]),
    mixed_flatten([[a,b,c],[d,[e,f]]], [a,b,c,d,[e,f]]).

% deep_flatten(L, R).
test_deep_flatten :-
    deep_flatten([[a,b,c],[d,e,f]], [a,b,c,d,e,f]),
    deep_flatten([[a,b],c,d], [a,b,c,d]),
    deep_flatten([[a,b,c],[d,[e,f]]], [a,b,c,d,e,f]),
    deep_flatten([[a,[b,c,[d,[e,f]],[g,h]]],[i],j], [a,b,c,d,e,f,g,h,i,j]).

% clear_empty_list(L, R).
test_clear_empty_list :-
    clear_empty_list([a,b,[],c,d,[]], [a,b,c,d]),
    clear_empty_list([[],[],[],[]], []),
    clear_empty_list([a,b,c],[a,b,c]).

% list_min(M, Min).
test_list_min :-
    list_min([4,7,5,3,7,2,4,7,8], 2),
    list_min([6,4,8,5,7,4,1,0], 0).

% list_max(M, Max).
test_list_max :-
    list_max([4,7,5,3,7,2,4,7,8], 8),
    list_max([6,4,3,5,7,4,1,0], 7).

% index(L, E, I).
test_index :-
    index([a,b,a,a,c,b,b,c,a,a,b,c,a,c,b], c, 4),
    \+ index([a,b,a,a,c,b,b,c,a,a,b,c,a,c,b], c, 7),
    \+ index([a,b,a,a,c,b,b,c,a,a,b,c,a,c,b], c, 11),
    \+ index([a,b,a,a,c,b,b,c,a,a,b,c,a,c,b], c, 13),
    index([a,b,a,a,b,b,a,c,b,a,a,b,c,a], c, 7),
    \+ index([a,b,a,a,b,b,a,c,b,a,a,b,c,a], c, 12),
    index([a,b,c,d], b, 1),
    \+ index([a,b,a,a,b,b], c, _),
    \+ index([], _, _).

% last_index(L, E, I).
test_last_index :-
    \+ last_index([a,b,a,a,c,b,b,c,a,a,b,c,a,c,b], c, 4),
    \+ last_index([a,b,a,a,c,b,b,c,a,a,b,c,a,c,b], c, 7),
    \+ last_index([a,b,a,a,c,b,b,c,a,a,b,c,a,c,b], c, 11),
    last_index([a,b,a,a,c,b,b,c,a,a,b,c,a,c,b], c, 13),
    \+ last_index([a,b,a,a,b,b,a,c,b,a,a,b,c,a], c, 7),
    last_index([a,b,a,a,b,b,a,c,b,a,a,b,c,a], c, 12),
    last_index([a,b,c,d], b, 1),
    \+ last_index([a,b,a,a,b,b], c, _),
    \+ last_index([], _, _).

% indices(L, E, I).
test_indices :-
    indices([a,b,a,a,c,b,b,c,a,a,b,c,a,c,b], c, 4),
    indices([a,b,a,a,c,b,b,c,a,a,b,c,a,c,b], c, 7),
    indices([a,b,a,a,c,b,b,c,a,a,b,c,a,c,b], c, 11),
    indices([a,b,a,a,c,b,b,c,a,a,b,c,a,c,b], c, 13),
    indices([a,b,a,a,b,b,a,c,b,a,a,b,c,a], c, 7),
    indices([a,b,a,a,b,b,a,c,b,a,a,b,c,a], c, 12),
    indices([a,b,c,d], b, 1),
    \+ indices([a,b,a,a,b,b], c, _),
    \+ indices([], _, _).

% *_*_suchthat(L, F, *, I).
test_suchthat :- 
    index_suchthat([0,b,_,0,b,_,a,7,q,_,s,y], var, 2),
    \+ index_suchthat([0,b,_,0,b,_,a,7,q,_,s,y], var, 5),
    \+ index_suchthat([0,b,_,0,b,_,a,7,q,_,s,y], var, 9),
    \+ last_index_suchthat([0,b,_,0,b,_,a,7,q,_,s,y], var, 2),
    \+ last_index_suchthat([0,b,_,0,b,_,a,7,q,_,s,y], var, 5),
    last_index_suchthat([0,b,_,0,b,_,a,7,q,_,s,y], var, 9),
    indices_suchthat([0,b,_,0,b,_,a,7,q,_,s,y], var, 2),
    indices_suchthat([0,b,_,0,b,_,a,7,q,_,s,y], var, 5),
    indices_suchthat([0,b,_,0,b,_,a,7,q,_,s,y], var, 9),
    a_index_suchthat([0,b,_,0,b,_,a,7,q,_,s,y], char_uppercase, 'B', 1),
    \+ a_index_suchthat([0,b,_,0,b,_,a,7,q,_,s,y], char_uppercase, 'B', 4),
    \+ a_index_suchthat([0,b,_,0,b,_,a,7,q,_,s,y], char_uppercase, 'B', 5),
    \+ a_last_index_suchthat([0,b,_,0,b,_,a,7,q,_,s,y], char_uppercase, 'B', 1),
    \+ a_last_index_suchthat([0,b,_,0,b,_,a,7,q,_,s,y], char_uppercase, 'B', 4),
    a_last_index_suchthat([0,b,_,0,b,_,a,7,q,_,s,y], char_uppercase, 'B', 9),
    a_indices_suchthat([0,b,_,0,b,_,a,7,q,_,s,y], char_uppercase, 'B', 1),
    a_indices_suchthat([0,b,_,0,b,_,a,7,q,_,s,y], char_uppercase, 'B', 4),
    a_indices_suchthat([0,b,_,0,b,_,a,7,q,_,s,y], char_uppercase, 'B', 9),
    l_index_suchthat([0,b,_,0,a,_,a,7,q,_,s,y], char_uppercase, ['A'], 2),
    \+ l_index_suchthat([0,b,_,0,a,_,a,7,q,_,s,y], char_uppercase, ['A'], 4),
    \+ l_index_suchthat([0,b,_,0,a,_,a,7,q,_,s,y], char_uppercase, ['A'], 5),
    \+ l_last_index_suchthat([0,b,_,0,a,_,a,7,q,_,s,y], char_uppercase, ['A'], 2),
    \+ l_last_index_suchthat([0,b,_,0,a,_,a,7,q,_,s,y], char_uppercase, ['A'], 4),
    l_last_index_suchthat([0,b,_,0,a,_,a,7,q,_,s,y], char_uppercase, ['A'], 9),
    l_indices_suchthat([0,b,_,0,a,_,a,7,q,_,s,y], char_uppercase, ['A'], 4),
    l_indices_suchthat([0,b,_,0,a,_,a,7,q,_,s,y], char_uppercase, ['A'], 6),
    l_indices_suchthat([0,b,_,0,a,_,a,7,q,_,s,y], char_uppercase, ['A'], 9).

test_lists :- test_all([
    test_list_get,
    test_list_set,
    test_prefix,
    test_proper_prefix,
    test_suffix,
    test_proper_suffix,
    test_sublist,
    test_sublist_n,
    test_join,
    test_push_front,
    test_push_back,
    test_pop_front,
    test_pop_back,
    test_fill_n,
    test_iota,
    test_range,
    test_range_n,
    test_consecutive,
    test_reverse,
    test_map,
    test_l_map,
    test_flatten,
    test_mixed_flatten,
    test_deep_flatten,
    test_clear_empty_list,
    test_list_min,
    test_list_max,
    test_index,
    test_last_index,
    test_indices,
    test_suchthat
]).

:- test_lists.