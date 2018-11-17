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
 * game(?Board, ?Wc, ?Bc, ?next).
 *   A game of Pente.
 *   > The current Board is represented by a 19x19 matrix, consisting of
 *     characters c for empty slots, w for White's pieces and b for Black's pieces.
 *   > Wc and Bc are White's and Black's captures, respectively.
 *   > next is w or b, indicating whose turn it is to play.
 */
game(_, _, _, _).

/**
 * start_game/3
 * start_game(+S, player, player)
 *   Starts a player vs player pente game with a board with size S
 */
start_game(S, player, player) :-
	  make_board(S, B),
		game_loop(game(B, 0, 0, w), S).

/**
 * game_loop(+game(B, Pw, Pb, Next))
 *   Next iteration of the game 
 */

game_loop(game(B, Wc, Bc, Next), Size) :-
	display_game(B, Wc, Bc, Next),
	read_position(RepRow, RepCol),
	rep_internal(Size, [RepRow, RepCol], [R, C]),
	move([R, C], game(B, Wc, Bc, Next), game(NewBoard, Nwc, Nbc, Nnext)),
	game_loop_aux(game(NewBoard, Nwc, Nbc, Nnext), Size).

game_loop_aux(game(B, Wc, Bc, _), _) :-
	is_player(P),
	game_over(game(B, Wc, Bc, _), P), !,
	display_game(B, Wc, Bc, P),
	victory(P).

game_loop_aux(game(B, Wc, Bc, Next), Size) :- game_loop(game(B, Wc, Bc, Next), Size).

/**
 * victory(P)
 * displays a victory message for the player P (w or b)
 */
victory(w):- write('White player won!'), nl.
victory(b):- write('Black player won!'), nl.

s_g(S):- start_game(S, player, player).

/**
 * game_over/2
 * game_over(+game(Board, White, Black, next), ?P).
 *   Verifies if the game is over with winner P (w or b).
 */
game_over(game(Board, Wc, _Bc, _Next), w) :-
    five_board(Board, w);
    Wc >= 10.
game_over(game(Board, _Wc, Bc, _Next), b) :-
    five_board(Board, b); 
    Bc >= 10.
