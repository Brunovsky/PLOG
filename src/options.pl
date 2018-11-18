/**
 * Game options
 */

/**
 * sanitize_options/2
 * sanitize_options(+Options, -NewOptions).
 *   Sanitizes a given Options list into NewOptions.
 */
sanitize_options(Options, NewOptions) :- 
    sanitize_board_size(Options, BoardSize), !,
    sanitize_difficulty(Options, DDepth, DPadding, DWidth), !,
    sanitize_depth(Options, DDepth, Depth), !,
    sanitize_padding(Options, DPadding, Padding), !,
    sanitize_width(Options, DWidth, Width), !,
    NewOptions = [board_size(BoardSize), depth(Depth), padding(Padding), width(Width)].

/**
 * sanitize_board_size/2
 * sanitize_board_size(+Options, -BoardSize).
 *   Sanitizes Options' game_board option into BoardSize.
 */
sanitize_board_size(Options, BoardSize) :-
    getopt(Options, board_size, 19, BoardSize),
    integer(BoardSize),
    1 is mod(BoardSize, 2);
    write('Invalid BOARD_SIZE option!'), nl, fail.

/**
 * sanitize_difficulty/4
 * sanitize_difficulty(+Options, -DDepth, -DPadding, -DWidth).
 *   Sanitizes Options' difficulty option into its 3 components (Depth, Padding, Width).
 */
sanitize_difficulty(Options, DDepth, DPadding, DWidth) :-
    getopt(Options, difficulty, 3, Difficulty),
    integer(Difficulty),
    Difficulty > 0,
    5 >= Difficulty,
    sanitize_difficulty_aux(Difficulty, DDepth, DPadding, DWidth);
    write('Invalid DIFFICULTY option!'), nl, fail.

sanitize_difficulty_aux(1, 1, 7, [2]).
sanitize_difficulty_aux(2, 2, 5, [5,3]).
sanitize_difficulty_aux(3, 4, 3, [10,5,3,2]).
sanitize_difficulty_aux(4, 5, 3, [10,7,5]).
sanitize_difficulty_aux(5, 6, 3, [10, 10, 8]).

/**
 * sanitize_depth/3
 * sanitize_depth(+Options, +DDepth, -Depth).
 *   Sanitizes Options' depth option into Depth using DDepth as default.
 */
sanitize_depth(Options, DDepth, Depth) :-
    getopt(Options, depth, DDepth, Depth),
    integer(Depth),
    Depth > 0;
    write('Invalid DEPTH option!'), nl, fail.

/**
 * sanitize_padding/3
 * sanitize_padding(+Options, +DPadding, -Padding).
 *   Sanitizes Options' padding option into Padding using DPadding as default.
 */
sanitize_padding(Options, DPadding, Padding) :-
    getopt(Options, padding, DPadding, Padding),
    integer(Padding),
    Padding >= 0;
    write('Invalid PADDING option!'), nl, fail.

/**
 * sanitize_width/3
 * sanitize_width(+Options, +DWidth, -Width).
 *   Sanitizes Options' width option into Width using DWidth as default.
 */
sanitize_width(Options, DWidth, Width) :-
    getopt(Options, width, DWidth, Temp),
    (is_list(Temp), length(Temp, L), L > 0, Width = Temp;
    integer(Temp), Width = [Temp]);
    write('Invalid WIDTH option!'), nl, fail.


/**
 * tree_parseopt/2, next_depth/2
 * tree_parseopt(+Options, -NewOptions).
 * next_depth(+[Depth,...], -[Depth-1,...]).
 */
tree_parseopt(Options, NewOptions) :-
    getopt(Options, depth, 4, TotalDepth),
    getopt(Options, padding, 3, Padding),
    getopt(Options, width, [10,5,3,2], WidthList),
    NewOptions = [current(0), depth(TotalDepth), padding(Padding), width(WidthList)].

/**
 * depth_width/3, next_depth/2
 * depth_width(+WidthList, +Depth, -Width).
 * next_depth(+Options, -NewOptions).
 */
depth_width(WidthList, Depth, Width) :-
    length(WidthList, Length),
    Depth >= Length,
    last(WidthList, Width);
    nth0(Depth, WidthList, Width).

next_depth(Options, NewOptions) :-
    opt_widthlist(Options, WidthList),
    opt_padding(Options, Padding),
    opt_depth(Options, Depth),
    opt_totaldepth(Options, TotalDepth),
    D is Depth + 1,
    NewOptions = [current(D), depth(TotalDepth), padding(Padding), width(WidthList)].

/**
 * opt_padding/2, opt_depth/2, opt_totaldepth/2, opt_widthlist/2, opt_width/3
 *   Calls to getopt/3
 */
opt_padding(Options, Padding) :-
    getopt(Options, padding, Padding).

opt_depth(Options, Depth) :-
    getopt(Options, current, Depth).

opt_totaldepth(Options, TotalDepth) :-
    getopt(Options, depth, TotalDepth).

opt_widthlist(Options, WidthList) :-
    getopt(Options, width, WidthList).

opt_width(Options, Width) :-
    opt_depth(Options, Depth),
    opt_widthlist(Options, WidthList),
    depth_width(WidthList, Depth, Width).
