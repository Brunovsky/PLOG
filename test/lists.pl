% fill_n(N, Elem, List).
test_fill_n :-
    fill_n(4, a, [a,a,a,a]),
    fill_n(7, b, [b,b,b,b,b,b,b]),
    fill_n(2, 0, [0,0]),
    fill_n(0, a, []),
    fill_n(3, [a,X], [[a,X],[a,X],[a,X]]),
    \+ fill_n(3, a, [a,a,a,a]),
    \+ fill_n(5, c, [c,c,c]).

% range(List, I, Part).
test_range :-
    range([0,1,2,3,4,5], [3,5], [2,3,4]),
    range([a,b,c,d,e,f,g], [2,6], [b,c,d,e,f]),
    \+ range([a,b,c,d], [3,10], [c,d]),
    \+ range([a,b,c], [4,5], []),
    range([0,1,2,3,4,5], 4, [3,4,5]),
    range([a,b,c,d,e,f,g], 5, [e,f,g]).

% consecutive(List, Elem, N).
test_consecutive :-
    consecutive([0,1,3,3,2,4,2,3,3,3,7,4], 3, 3),
    %\+ consecutive([0,0,3,3,4,9,7,3,4,3,0,3,3,3,3,5], 3, 5),
    %consecutive([6,4,2,8,3,4,4,5,8,8,9,9,9,8,1,2,3,5,6,8,8,2,3,9], 9, 3),
    %consecutive([3,2,4,4,4,4,2,0], 4, 4),
    %\+ consecutive([3,2,4,4,4,2,0], 4, 4),
    \+ consecutive([0,1], a, 5).

% map(P, List, R).
test_map :-
    map(length, [[a,b,c],[d,e],[f,g,h,i]], [3,2,4]),
    map(last, [[a,b,c],[d,e],[f,g,h,i]], [c,e,i]),
    map(reverse, [[a,b,c],[d,e,f]], [[c,b,a],[f,e,d]]),
    map(_, [], []).

% l_map(P, List, Args, R).
test_l_map :-
    lb_map(index, [a], [[a,b,a],[b,b,a,b],[c,c,b,a]], [1,3,4]),
    b_map(fill_n, a, [3,4,2], [[a,a,a],[a,a,a,a],[a,a]]),
    lb_map(range, [[2,3]], [[a,b,c,d],[e,f,g,h]], [[b,c],[f,g]]).

% flatten(List, Flat).
test_flatten :-
    flatten([[a,b,c],[d,e,f]], [a,b,c,d,e,f]),
    \+ flatten([[a,b],c,d], _),
    flatten([[a,b,c],[d,[e,f]]], [a,b,c,d,[e,f]]).

% mixed_flatten(List, Flat).
test_mixed_flatten :-
    mixed_flatten([[a,b,c],[d,e,f]], [a,b,c,d,e,f]),
    mixed_flatten([[a,b],c,d], [a,b,c,d]),
    mixed_flatten([[a,b,c],[d,[e,f]]], [a,b,c,d,[e,f]]).

% deep_flatten(List, Flat).
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

% index(List, Elem, I).
test_index :-
    index([a,b,a,a,c,b,b,c,a,a,b,c,a,c,b], c, 5),
    \+ index([a,b,a,a,c,b,b,c,a,a,b,c,a,c,b], c, 8),
    \+ index([a,b,a,a,c,b,b,c,a,a,b,c,a,c,b], c, 12),
    \+ index([a,b,a,a,c,b,b,c,a,a,b,c,a,c,b], c, 14),
    index([a,b,a,a,b,b,a,c,b,a,a,b,c,a], c, 8),
    \+ index([a,b,a,a,b,b,a,c,b,a,a,b,c,a], c, 13),
    index([a,b,c,d], b, 2),
    \+ index([a,b,a,a,b,b], c, _),
    \+ index([], _, _).

% last_index(List, Elem, I).
test_last_index :-
    \+ last_index([a,b,a,a,c,b,b,c,a,a,b,c,a,c,b], c, 5),
    \+ last_index([a,b,a,a,c,b,b,c,a,a,b,c,a,c,b], c, 8),
    \+ last_index([a,b,a,a,c,b,b,c,a,a,b,c,a,c,b], c, 12),
    last_index([a,b,a,a,c,b,b,c,a,a,b,c,a,c,b], c, 14),
    \+ last_index([a,b,a,a,b,b,a,c,b,a,a,b,c,a], c, 8),
    last_index([a,b,a,a,b,b,a,c,b,a,a,b,c,a], c, 13),
    last_index([a,b,c,d], b, 2),
    \+ last_index([a,b,a,a,b,b], c, _),
    \+ last_index([], _, _).

% indices(List, Elem, I).
test_indices :-
    indices([a,b,a,a,c,b,b,c,a,a,b,c,a,c,b], c, 5),
    indices([a,b,a,a,c,b,b,c,a,a,b,c,a,c,b], c, 8),
    indices([a,b,a,a,c,b,b,c,a,a,b,c,a,c,b], c, 12),
    indices([a,b,a,a,c,b,b,c,a,a,b,c,a,c,b], c, 14),
    indices([a,b,a,a,b,b,a,c,b,a,a,b,c,a], c, 8),
    indices([a,b,a,a,b,b,a,c,b,a,a,b,c,a], c, 13),
    indices([a,b,c,d], b, 2),
    \+ indices([a,b,a,a,b,b], c, _),
    \+ indices([], _, _).

% *_*_suchthat(P, List, Args, I).
test_suchthat :-
    index_suchthat(var, [0,b,_,0,b,_,a,7,q,_,s,y], 3),
    \+ index_suchthat(var, [0,b,_,0,b,_,a,7,q,_,s,y], 6),
    \+ index_suchthat(var, [0,b,_,0,b,_,a,7,q,_,s,y], 10),
    \+ last_index_suchthat(var, [0,b,_,0,b,_,a,7,q,_,s,y], 3),
    \+ last_index_suchthat(var, [0,b,_,0,b,_,a,7,q,_,s,y], 6),
    last_index_suchthat(var, [0,b,_,0,b,_,a,7,q,_,s,y], 10),
    indices_suchthat(var, [0,b,_,0,b,_,a,7,q,_,s,y], 3),
    indices_suchthat(var, [0,b,_,0,b,_,a,7,q,_,s,y], 6),
    indices_suchthat(var, [0,b,_,0,b,_,a,7,q,_,s,y], 10),
    a_index_suchthat(char_uppercase, [0,b,_,0,b,_,a,7,q,_,s,y], 'B', 2),
    \+ a_index_suchthat(char_uppercase, [0,b,_,0,b,_,a,7,q,_,s,y], 'B', 5),
    \+ a_index_suchthat(char_uppercase, [0,b,_,0,b,_,a,7,q,_,s,y], 'B', 6),
    \+ a_last_index_suchthat(char_uppercase, [0,b,_,0,b,_,a,7,q,_,s,y], 'B', 2),
    \+ a_last_index_suchthat(char_uppercase, [0,b,_,0,b,_,a,7,q,_,s,y], 'B', 5),
    a_last_index_suchthat(char_uppercase, [0,b,_,0,b,_,a,7,q,_,s,y], 'B', 10),
    a_indices_suchthat(char_uppercase, [0,b,_,0,b,_,a,7,q,_,s,y], 'B', 2),
    a_indices_suchthat(char_uppercase, [0,b,_,0,b,_,a,7,q,_,s,y], 'B', 5),
    a_indices_suchthat(char_uppercase, [0,b,_,0,b,_,a,7,q,_,s,y], 'B', 10),
    l_index_suchthat(char_uppercase, [0,b,_,0,a,_,a,7,q,_,s,y], ['A'], 3),
    \+ l_index_suchthat(char_uppercase, [0,b,_,0,a,_,a,7,q,_,s,y], ['A'], 5),
    \+ l_index_suchthat(char_uppercase, [0,b,_,0,a,_,a,7,q,_,s,y], ['A'], 6),
    \+ l_last_index_suchthat(char_uppercase, [0,b,_,0,a,_,a,7,q,_,s,y], ['A'], 3),
    \+ l_last_index_suchthat(char_uppercase, [0,b,_,0,a,_,a,7,q,_,s,y], ['A'], 5),
    l_last_index_suchthat(char_uppercase, [0,b,_,0,a,_,a,7,q,_,s,y], ['A'], 10),
    l_indices_suchthat(char_uppercase, [0,b,_,0,a,_,a,7,q,_,s,y], ['A'], 5),
    l_indices_suchthat(char_uppercase, [0,b,_,0,a,_,a,7,q,_,s,y], ['A'], 7),
    l_indices_suchthat(char_uppercase, [0,b,_,0,a,_,a,7,q,_,s,y], ['A'], 10).


test_lists :- write('==[TESTS]== lists'), nl, test_all([
    test_fill_n,
    test_range,
    test_consecutive,
    test_map,
    test_l_map,
    test_flatten,
    test_mixed_flatten,
    test_deep_flatten,
    test_clear_empty_list,
    test_index,
    test_last_index,
    test_indices,
    test_suchthat
]).
