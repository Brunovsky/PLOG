/**
 * Options for TREE EVALUATION in value.pl
 */

/**
 * tree_parseopt/2, next_depth/2
 * tree_parseopt(+Options, -NewOptions).
 * next_depth(+[Depth,...], -[Depth-1,...]).
 */
tree_parseopt(Options, NewOptions) :-
    getopt(Options, depth, 4, TotalDepth),
    getopt(Options, padding, 3, Padding),
    getopt(Options, width, [10,5,3,2], WidthList),
    NewOptions = [current(0), depth(TotalDepth), padding(Padding), width(WidthList)], !.

/**
 * depth_width/3, next_depth/2
 * depth_width(+WidthList, +Depth, -Width).
 * next_depth(+Options, -NewOptions).
 */
depth_width(WidthList, Depth, Width) :-
    length(WidthList, Length),
    Depth >= Length,
    last(WidthList, Width), !;
    nth0(Depth, WidthList, Width), !.

next_depth(Options, NewOptions) :-
    opt_widthlist(Options, WidthList),
    opt_padding(Options, Padding),
    opt_depth(Options, Depth),
    opt_totaldepth(Options, TotalDepth),
    D is Depth + 1,
    NewOptions = [current(D), depth(TotalDepth), padding(Padding), width(WidthList)], !.

/**
 * opt_padding/2, opt_depth/2, opt_totaldepth/2, opt_widthlist/2, opt_width/3
 *   Calls to getopt/3
 */
opt_padding(Options, Padding) :-
    getopt(Options, padding, Padding), !.

opt_depth(Options, Depth) :-
    getopt(Options, current, Depth), !.

opt_totaldepth(Options, TotalDepth) :-
    getopt(Options, depth, TotalDepth), !.

opt_widthlist(Options, WidthList) :-
    getopt(Options, width, WidthList), !.

opt_width(Options, Width) :-
    opt_depth(Options, Depth),
    opt_widthlist(Options, WidthList),
    depth_width(WidthList, Depth, Width).
