% build_start_node/2
test_build_start_node :-
    Board = [
        [c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c],
        [c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c],
        [c,c,c,c,c,c,c,c,c,c,w,c,c,c,c,c,c,c,c],
        [c,c,c,c,c,c,c,c,w,b,b,b,b,w,c,c,c,c,c],
        [c,c,c,c,c,c,c,c,b,c,w,c,c,c,c,c,c,c,c],
        [c,c,c,c,c,c,c,b,w,w,w,w,b,c,c,c,c,c,c],
        [c,c,c,c,c,c,b,c,w,b,c,c,w,c,c,c,c,c,c],
        [c,c,c,c,b,w,w,w,w,b,c,c,c,c,c,c,c,c,c],
        [c,c,c,c,c,c,b,c,w,b,c,c,c,c,c,c,c,c,c],
        [c,c,c,c,c,c,w,c,b,w,b,c,c,c,c,c,c,c,c],
        [c,c,c,c,c,c,c,c,c,c,c,b,c,c,c,c,c,c,c],
        [c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c],
        [c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c],
        [c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c],
        [c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c],
        [c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c],
        [c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c],
        [c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c],
        [c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c]
    ],
    build_start_node(Board, b, [0,0], Node),
    %print_node(Node), nl,
    Node = node(Board, b, Val, [0,0], [], Total),
    evaluate_board(Board, Val),
    totalval(Val, [0,0], Total).

% build_child_node/3
test_build_child_node :-
    Board = [
        [c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c],
        [c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c],
        [c,c,c,c,c,c,c,c,c,c,w,c,c,c,c,c,c,c,c],
        [c,c,c,c,c,c,c,c,w,b,b,b,b,w,c,c,c,c,c],
        [c,c,c,c,c,c,c,c,b,c,w,c,c,c,c,c,c,c,c],
        [c,c,c,c,c,c,c,b,w,w,w,w,b,c,c,c,c,c,c],
        [c,c,c,c,c,c,b,c,w,b,c,c,w,c,c,c,c,c,c],
        [c,c,c,c,b,w,w,w,w,b,c,c,c,c,c,c,c,c,c],
        [c,c,c,c,c,c,b,c,w,b,c,c,c,c,c,c,c,c,c],
        [c,c,c,c,c,c,w,c,b,w,b,c,c,c,c,c,c,c,c],
        [c,c,c,c,c,c,c,c,c,c,c,b,c,c,c,c,c,c,c],
        [c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c],
        [c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c],
        [c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c],
        [c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c],
        [c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c],
        [c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c],
        [c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c],
        [c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c]
    ],
    place_stone(b, Board, [12,13], NewBoard, _), !, % does not capture
    build_start_node(Board, b, [0,0], Node),
    %print_node(Node), nl, !,
    build_start_node(NewBoard, w, [0,0], NewNode), !,
    %print_node(NewNode), nl,
    build_child_node([12,13], Node, ChildNode),
    %print_node(ResultNode), nl,
    ChildNode = NewNode.

% order_children/3

test_tree :- test_all(tree, [
    test_build_start_node,
    test_build_child_node
]).
