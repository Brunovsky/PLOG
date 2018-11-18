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
    Node = node(Board, P, Val, Cap, Children, Worth),
    write('===== ===== ===== ===== node/6 ===== ===== ===== ====='), nl,
    display_game(Board, P, Cap),
    print_val(Val), nl,
    format('Node Worth: ~D', Worth), nl,
    matrix_length(Board, RowSize, _),
    print_children(Children, RowSize), !.

/**
 * print_children/1
 * print_children(+Children).
 *   For debugging purposes only.
 */
print_children(Children) :- print_children(Children, 19).

print_children(Children, RowSize) :-
    length(Children, C),
    format('===== ===== Children: ~d ===== =====', C), nl,
    (   foreach(Worth-(Move-_), Children),
        param(RowSize)
    do  format('  Worth ~D~n  Move: ~w~n', [Worth, Move])
    ), !.

/**
 * print_moves/1
 * print_moves(+Node).
 */
print_moves(Moves) :- print_moves(Moves, 19).

print_moves(Moves, RowSize) :-
    (   foreach(Move, Moves),
        param(RowSize)
    do  (   rep_internal(RowSize, [RepRow,RepCol], Move),
            format('~w~d  ', [RepCol,RepRow])
        )
    ), !.

/**
 * Accessors and other quick utilities.
 */
node_board(node(Board, _, _, _, _, _), Board).
node_player(node(_, P, _, _, _, _), P).
node_val(node(_, _, Val, _, _, _), Val).
node_cap(node(_, _, _, Cap, _, _), Cap).
node_children(node(_, _, _, _, Children, _), Children).
node_worth(node(_, _, _, _, _, Worth), Worth).
node_bestchild(node(_, _, _, _, [Child|_], _), Child).

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
    place_stone(P, Board, Move, ChildBoard, Captures), !,
    add_captures(P, Captures, Cap, ChildCap),
    reevaluate_board(Board, ChildBoard, Val, ChildVal), !,
    %evaluate_board(ChildBoard, ChildVal), !,
    totalval(ChildVal, ChildCap, Worth), !.

/**
 * order_children/3
 * order_children(+P, +Children, -OrderedChildren).
 *   Order a list [V-([Ri,Ci]-node/6),...] according to key V.
 */
order_children(w, Children, OrderedChildren) :-
    keysort(Children, BlackOrdered),
    reverse(BlackOrdered, OrderedChildren).

order_children(b, Children, OrderedChildren) :-
    keysort(Children, OrderedChildren).

/**
 * organize_children/[4,5]
 * organize_children(+P, +Unordered, -Ordered, -BestWorth).
 * organize_children(+P, +Width, +Unordered, -Ordered, -BestWorth).
 *   Order a list of children and deduce the highest worth.
 */
organize_children(P, Unordered, Ordered, BestWorth) :-
    order_children(P, Unordered, Ordered), !,
    head(Ordered, BestChild),
    child_value(BestChild, BestWorth), !.

organize_children(P, Width, Unordered, BestOrdered, BestWorth) :-
    order_children(P, Unordered, Ordered), !,
    extra_prefix_length(Ordered, BestOrdered, Width),
    head(BestOrdered, BestChild),
    child_value(BestChild, BestWorth), !.

/**
 * choose_winner_if/4
 * choose_winner_if(+P, +Child, +Children, -ChildOrChildren).
 */
choose_winner_if(P, _, [Worth-(Move-Child)|_], [Worth-(Move-Child)]) :-
    winning_value(P, Worth), !.

choose_winner_if(P, Worth-(Move-Child), _, [Worth-(Move-Child)]) :-
    winning_value(P, Worth), !.

choose_winner_if(_, Child, Children, [Child|Children]).

/**
 * choose_best_entry/4
 * choose_best_entry(+P, +Child1, +Child2, -Best).
 */
choose_best_entry(P, Child1, Child2, Child1) :-
    Child1 = Worth1-(_-_),
    Child2 = Worth2-(_-_),
    best_value(P, Worth1, Worth2, Worth1), !.

choose_best_entry(P, Child1, Child2, Child2) :-
    Child1 = Worth1-(_-_),
    Child2 = Worth2-(_-_),
    best_value(P, Worth1, Worth2, Worth2).

/**
 * build_children_loop/3
 * build_children_loop(Node, MoveList, Children).
 */
% no more children
build_children_loop(_, [], []).

build_children_loop(Node, [Move|RestMoves], ResultChildren) :-
    node_player(Node, P),
    build_child_node(Move, Node, Child), !,
    node_worth(Child, Worth),
    ChildEntry = Worth-(Move-Child),
    (   winning_value(P, Worth) ->
        ResultChildren = [ChildEntry];
        build_children_loop(Node, RestMoves, RestChildren),
        choose_winner_if(P, ChildEntry, RestChildren, ResultChildren)
    ), !.

/**
 * build_children/3
 * build_children(+Node, -NewNode, +Options).
 *   Constructs a list [V-([Ri,Ci]-node/6),...] for a Node without children,
 *   no recursion.
 *   See tree_parseopt/3 for Options.
 */
build_children(Node, NewNode, Options) :-
    opt_turn(Options, Turn),
    opt_padding(Options, Padding),
    opt_width(Options, Width),
    opt_tournament(Options, Tournament),
    Node = node(Board, P, Val, Cap, _, _),
    NewNode = node(Board, P, Val, Cap, Children, NewWorth), !,
    valid_moves_within_boundary(Board, Padding, Turn, Tournament, ListOfMoves), !,
    build_children_loop(Node, ListOfMoves, Unordered), !,
    organize_children(P, Width, Unordered, Children, NewWorth), !.

/**
 * recurse_children_loop/3
 * recurse_children_loop(+Node, MoveList, Children).
 */
recurse_children_loop(Node, [ChildEntry], ResultChildren, Options, NewChildEntry) :-
    node_player(Node, P),
    ChildEntry = _-(Move-Child), !,
    build_tree(Child, NewChild, Options), !,
    node_worth(NewChild, NewWorth),
    NewChildEntry = NewWorth-(Move-NewChild),
    (   % include this entry if it isn't losing
        losing_value(P, NewWorth) ->
        ResultChildren = [];
        ResultChildren = [NewChildEntry]
    ), !.

recurse_children_loop(Node, [ChildEntry|OldChildren], ResultChildren, Options, Best) :-
    node_player(Node, P),
    ChildEntry = _-(Move-Child), !,
    build_tree(Child, NewChild, Options), !,
    node_worth(NewChild, NewWorth),
    NewChildEntry = NewWorth-(Move-NewChild),
    (   % include only this child
        winning_value(P, NewWorth) ->
        ResultChildren = [NewChildEntry],
        Best = NewChildEntry;
        % discard this child
        losing_value(P, NewWorth) ->
        recurse_children_loop(Node, OldChildren, ResultChildren, Options, OldBest),
        choose_best_entry(P, OldBest, NewChildEntry, Best);
        % recurse naturally
        recurse_children_loop(Node, OldChildren, RestChildren, Options, OldBest),
        choose_winner_if(P, NewChildEntry, RestChildren, ResultChildren),
        choose_best_entry(P, OldBest, NewChildEntry, Best)
    ), !.

/**
 * recurse_children/3
 * recurse_children(+Node, -NewNode, +Options).
 *   From a list [V-([Ri,Ci]-node/6)] of children leafs, construct a new node
 *   whose children have been recursed according to the given options.
 *   See build_tree/3 for Options.
 */
recurse_children(Node, Node, _) :-
    Node = node(_, _, _, _, [], _), !.

recurse_children(Node, Node, _) :-
    Node = node(_, P, _, _, [Worth-(_-_)], Worth),
    winning_value(P, Worth), !.

recurse_children(Node, NewNode, Options) :-
    next_depth(Options, OptionsChildren),
    Node = node(Board, P, Val, Cap, OldChildren, _),
    NewNode = node(Board, P, Val, Cap, NewChildren, BestWorth), !,
    recurse_children_loop(Node, OldChildren, Unordered, OptionsChildren, Best), !,
    (   Unordered = [] ->
        NewChildren = [Best],
        Best = BestWorth-(_-_);
        organize_children(P, Unordered, NewChildren, BestWorth)
    ), !.

/**
 * build_tree/3
 * build_tree(+Node, -Tree, +Options).
 *   Builds an evaluation Tree starting at Node with given Options (calls itself).
 */
build_tree(Node, Node, Options) :-
    opt_depth(Options, Depth),
    opt_totaldepth(Options, Depth).

build_tree(Node, Tree, Options) :-
    build_children(Node, NodeWithChildren, Options),
    recurse_children(NodeWithChildren, Tree, Options).

/**
 * analyze_tree/2
 * analyze_tree(+Game, -Tree).
 */
analyze_tree(Game, Tree) :-
    Game = game(Board, P, Cap, Turn, UserOptions),
    tree_parseopt(UserOptions, Turn, Options),
    build_start_node(Board, P, Cap, Node),
    build_tree(Node, Tree, Options).

/**
 * choose_move/2
 * choose_move(+Tree, -[R,C]).
 *   Choose the best move [R,C] to a given Tree.
 */
choose_move(Tree, [R,C]) :-
    node_bestchild(Tree, Child),
    Child = _-([R,C]-_).
