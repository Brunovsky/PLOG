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
        [c,c,c,c,c,c,w,c,b,w,b,c,c,c,c,c,c,c,c], % 10
        [c,c,c,c,c,c,c,c,c,c,c,b,c,c,c,c,c,c,c],
        [c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c],
        [c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c],
        [c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c],
        [c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c],
        [c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c],
        [c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c],
        [c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c],
        [c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c]
        %                 10
    ],
    build_start_node(Board, b, [0,0], Node),
    Node = node(Board, b, Val, [0,0], [], Total),
    evaluate_board(Board, Val),
    totalval(Val, [0,0], Total),
    write(Node).



test_tree :- test_all(tree, [
    test_build_start_node
]).


/**
random_list(10, [c,c,w], Wpure),
random_list(10, [c,c,b], Bpure),
random_list(19, [c,c,w,b], Mixed),
random_list(19, [c,c,c,w,w,b], BiasedW),
random_list(19, [c,c,c,b,b,w], BiasedB)

evaluate(Wpure, W1),
evaluate(ReversalWpure, W2), W1 is -W2,
evaluate(Bpure, B1),
evaluate(ReversalBpure, B2), B1 is -B2,
evaluate(Mixed, M1),
evaluate(ReversalMixed, M2), M1 is -M2,
*/