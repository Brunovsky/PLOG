/**
 * ===== ===== ===== ===== EVALUATION TREE ===== ===== ===== =====
 *
 * Each node in the tree with be represented by the compound node/6:
 *           node(Board, P, val/4, [Wc,Bc], Children, Worth).
 *                                          ^ = [[Ri,Ci]-node/6, ...]
 * P is the next player to play.
 * val/4 is the value compound discussed earlier.
 * Wc and Bc are white and black's captures respectively.
 * Children is a list of Key-Value pairs, where the key is the move.
 * Worth is a float which represents the value of the node; for leaf nodes
 * without children this is computed by totalval/2.
 *
 * The behaviour of the evaluation tree can be tuned with an OptionsList
 *
 * Supported options:
 * 
 *     depth(D) D = 1,2,3,...
 *       Node depth of the tree.
 *
 *     padding(P) P = 0,1,2,...
 *       Padding of the board on new analysis recursion.
 *
 *     width([W1,W2,...]), Wi = 1,2,... for each i = 1,...,D.
 *       Or, for each depth i (top is depth 1) keep Wi children.
 *
 * See tree-opt.pl
 */

/**
 * print_node/1
 * print_node(+Node).
 *   For debugging purposes only.
 */
print_node(Node) :-
    Node = node(Board, P, Val, [Wc,Bc], Children, Worth),
    write('===== ===== ===== ===== node/6 ===== ===== ===== ====='), nl,
    display_game(Board, Wc, Bc, P),
    print_val(Val), nl,
    format('Node Worth: ~D', Worth), nl,
    length(Children, C),
    format('-- Children: ~d', C), nl,
    (   foreach(Value-(Move-_), Children)
    do  format('  Value ~D~n  Move: ~w~n', [Value, Move])
    ), !.

/**
 * Accessors and other quick utilities.
 */
node_board(node(Board, _, _, _, _, _), Board).
node_next(node(_, P, _, _, _, _), P).
node_val(node(_, _, Val, _, _, _), Val).
node_cap(node(_, _, _, Cap, _, _), Cap).
node_children(node(_, _, _, _, Children, _), Children).
node_worth(node(_, _, _, _, _, Worth), Worth).

child_value(Worth-(_-_), Worth).
child_move(_-(Move-_), Move).
child_node(_-(_-Child), Child).

/**
 * build_start_node/[2,4]
 * build_start_node(+Board, -Node).
 * build_start_node(+Board, +P, +Cap, -Node).
 *   Build a node/6 from scratch.
 */
build_start_node(Board, Node) :-
    build_start_node(Board, w, [0,0], Node), !.

build_start_node(Board, P, Cap, Node) :-
    Node = node(Board, P, Val, Cap, [], Total),
    evaluate_board(Board, Val),
    totalval(Val, Cap, Total), !.

/**
 * build_child_node/3
 * build_child_node(+Move, +ParentNode, -ChildNode).
 *   From a ParentNode and a move, constructs a ChildNode.
 */
build_child_node(Move, ParentNode, ChildNode) :-
    other_player(P, Other),
    ParentNode = node(Board, P, Val, Cap, _, _),
    ChildNode = node(ChildBoard, Other, ChildVal, ChildCap, [], Worth),
    place_stone(P, Board, Move, ChildBoard, Captures),
    add_captures(P, Captures, Cap, ChildCap),
    reevaluate_board(Board, ChildBoard, Val, ChildVal),
    totalval(ChildVal, ChildCap, Worth), !.

/**
 * order_children/3
 * order_children(+P, +Children, -OrderedChildren).
 *   Order a list [V-[Ri,Ci]-node/6,...] according to key V.
 */
order_children(w, Children, OrderedChildren) :-
    keysort(Children, BlackOrdered),
    reverse(BlackOrdered, OrderedChildren).

order_children(b, Children, OrderedChildren) :-
    keysort(Children, OrderedChildren).

/**
 * best_children/3
 * best_children(+Width, +Children, -BestChildren).
 *   Choose from Children only the best children (Width children only).
 */
best_children(Width, Children, BestChildren) :-
    extra_prefix_length(Children, BestChildren, Width).

/**
 * build_children/3
 * build_children(+Node, -NewNode, +Options).
 *   Constructs a list [V-([Ri,Ci]-node/6),...] for a Node without children,
 *   no recursion.
 *   See tree_parseopt/3 for Options.
 */
build_children(Node, NewNode, Options) :-
    opt_padding(Options, Padding),
    opt_width(Options, Width),
    Node = node(Board, P, Val, Cap, [], _),
    NewNode = node(Board, P, Val, Cap, Children, NewWorth),
    empty_positions_within_boundary(Board, Padding, ListOfMoves), % get subboard
    (   foreach(Move, ListOfMoves),
        fromto([], Childs, [Worth-(Move-Child)|Childs], Unordered),
        param(Node)
    do  (   build_child_node(Move, Node, Child),
            node_worth(Child, Worth)
        )
    ),
    order_children(P, Unordered, Ordered),
    best_children(Width, Ordered, Children), % filter children
    head(Children, BestChild),
    child_value(BestChild, NewWorth).

/**
 * recurse_children/3
 * recurse_children(+Node, -NewNode, +Options).
 *   From a list [V-([Ri,Ci]-node/6)] of children leafs, construct a new node
 *   whose children have been recursed according to the given options.
 *   See build_tree/3 for Options.
 */
recurse_children(Node, NewNode, Options) :-
    next_depth(Options, OptionsChildren),
    Node = node(Board, P, Val, Cap, Children, _),
    NewNode = node(Board, P, Val, Cap, NewChildren, NewWorth),
    % Recurse
    (   foreach(_-(Move-Child), Children),
        fromto([], NewChilds, [NewWorth-(Move-NewChild)|NewChilds], NewUnordered),
        param(OptionsChildren)
    do  (   build_tree(Child, NewChild, OptionsChildren),
            node_worth(NewChild, NewWorth)
        )
    ),
    order_children(P, NewUnordered, NewChildren),
    head(NewChildren, BestChild),
    child_value(BestChild, NewWorth).

/**
 * build_tree/3
 * build_tree(+Node, -Tree, +Options).
 *   Builds an evaluation Tree starting at Node with given Options (calls itself).
 */
build_tree(Node, Node, Options) :-
    getopt_depth(Options, 0).

build_tree(Node, Tree, Options) :-
    build_children(Node, NodeWithChildren, Options),
    recurse_children(NodeWithChildren, Tree, Options).

/**
 * analyze_tree/[4,5]
 * analyze_tree(+Board, +P, +Cap, -Tree).
 * analyze_tree(+Board, +P, +Cap, -Tree, +Options).
 */
analyze_tree(Board, P, Cap, Tree) :-
    analyze_tree(Board, P, Cap, Tree, []).

analyze_tree(Board, P, Cap, Tree, UserOptions) :-
    tree_parseopt(UserOptions, Options),
    build_start_node(Board, P, Cap, Node),
    build_tree(Node, Tree, Options).
