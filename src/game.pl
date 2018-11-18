pente :- 
	sanitize_options([], Options),
	main_menu(Options).

pente(Options) :-
	sanitize_options(Options, Sanitized),
	main_menu(Sanitized).

/**
 * player/2
 * player(?Color, ?Captures).
 *   A player of Pente, with a color, a certain number of Captures
 *   and a series of plays.
 * Color: w or b.
 * Captures: An int (between 0 and 10).
 */
player(_, _).

/**
 * other_player/2
 * other_player(?P1, ?P2).
 */
other_player(w, b).
other_player(b, w).

/**
 * is_player/1
 * is_player(?P).
 */
is_player(w).
is_player(b).

/**
 * game/4
 * game(?Board, ?Wc, ?Bc, ?next, ?Turn).
 *   A game of Pente.
 *   > The current Board is represented by a 19x19 matrix, consisting of
 *     characters c for empty slots, w for White's pieces and b for Black's pieces.
 *   > Wc and Bc are White's and Black's captures, respectively.
 *   > next is w or b, indicating whose turn it is to play.
 *   > Turn is the current game turn.
 */
game(_, _, _, _, _).

/**
 * start_game/3
 * start_game(player, player, +Options)
 * start_game(player, bot, +Options)
 *   Starts a pente game
 */
start_game(player, player, Options) :-
	getopt(Options, board_size, Size),
	make_board(Size, B),
	game_loop_1(game(B, 0, 0, w, 0), Options).

start_game(player, bot, Options) :-
	getopt(Options, board_size, Size),
	make_board(Size, B),
	game_loop_2(game(B, 0, 0, w, 0), Options).

start_game(bot, bot, Options) :-
	getopt(Options, board_size, Size),
	make_board(Size, B),
	game_loop_3(game(B, 0, 0, w, 0), Options).

/**
 * game_loop/2
 * game_loop(+game(B, Pw, Pb, Next), +Options)
 *   Next iteration of the game 
 */
game_loop_1(game(B, Wc, Bc, Next, Turn), Options) :-
	write('\e[2J'),
	display_game(B, Wc, Bc, Next),
	read_position(RepRow, RepCol),
	getopt(Options, board_size, Size),
	rep_internal(Size, [RepRow, RepCol], [R, C]),
	valid_move(B, Turn, [R, C]),
	move([R,C], game(B, Wc, Bc, Next, Turn), NewGame),
	game_loop_1_aux(NewGame, Options).

game_loop_1_aux(game(B, Wc, Bc, Next, Turn), Options) :-
	game_loop_aux(game(B, Wc, Bc, _, _), _);
	game_loop_1(game(B, Wc, Bc, Next, Turn), Options).

game_loop_2(game(B, Wc, Bc, w, Turn), Options) :-
	write('\e[2J'),
	display_game(B, Wc, Bc, w),
	read_position(RepRow, RepCol),
	getopt(Options, board_size, Size),
	rep_internal(Size, [RepRow, RepCol], [R, C]),
	valid_move(B, Turn, [R, C]),
	move([R,C], game(B, Wc, Bc, w, Turn), NewGame),
	game_loop_2_aux(NewGame, Options).

game_loop_2(game(B, Wc, Bc, b, Turn), Options) :-
	analyze_tree(B, b, [Wc,Bc], Tree, Options),
	choose_move(Tree, [R,C]),
	move([R,C], game(B, Wc, Bc, b, Turn), NewGame),
	game_loop_2_aux(NewGame, Options).

game_loop_2_aux(game(B, Wc, Bc, Next, Turn), Options) :-
	game_loop_aux(game(B, Wc, Bc, _, _), _);
	game_loop_2(game(B, Wc, Bc, Next, Turn), Options).

game_loop_3(game(B, Wc, Bc, Next, Turn), Options) :-
	write('\e[2J'),
	display_game(B, Wc, Bc, Next),
	analyze_tree(B, Next, [Wc,Bc], Tree, [turn(Turn)|Options]),
	choose_move(Tree, [R,C]),
	write(' '), get_code(_),
	move([R,C], game(B, Wc, Bc, Next, Turn), NewGame),
	game_loop_3_aux(NewGame, Options).

game_loop_3_aux(game(B, Wc, Bc, Next, Turn), Options) :-
	game_loop_aux(game(B, Wc, Bc, _, _), _);
	game_loop_3(game(B, Wc, Bc, Next, Turn), Options).

game_loop_aux(game(B, Wc, Bc, _, _), _) :-
	is_player(P),
	game_over(game(B, Wc, Bc, _, _), P), !,
	write('\e[2J'),
	display_game(B, Wc, Bc, P),
	victory(P).


/**
 * victory/1
 * victory(P)
 * displays a victory message for the player P (w or b)
 */
victory(w):- write('White player won!'), nl.
victory(b):- write('Black player won!'), nl.

/**
 * game_over/2
 * game_over(+game(Board, White, Black, next), ?P).
 *   Verifies if the game is over with winner P (w or b).
 */
game_over(game(Board, Wc, _Bc, _Next, _), w) :-
    five_board(Board, w);
    Wc >= 10.
game_over(game(Board, _Wc, Bc, _Next, _), b) :-
    five_board(Board, b); 
    Bc >= 10.
