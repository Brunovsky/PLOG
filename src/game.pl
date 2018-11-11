/**
 * player(?Color, ?Captures).
 *   A player of Pente, with a color, a certain number of Captures
 *   and a series of plays.
 * Color: w or b.
 * Captures: An int (between 0 and 10).
 */
player(_, _).

/**
 * other_player(+P1, ?P2).
 */
other_player(w, b).
other_player(b, w).

/**
 * is_player(?P).
 */
is_player(w).
is_player(b).

/**
 * game(?Board, ?White, ?Black, ?next).
 *   A game of Pente.
 *   > The current Board is represented by a 19x19 matrix, consisting of
 *     characters c for empty slots, w for White's pieces and b for Black's pieces.
 *   > White and Black are players.
 *   > next is w or b, indicating whose turn it is to play.
 */
game(_, _, _, _).

/**
* start_game(+S, player, player)
* 	Starts a player vs player pente game with a board with size S
*/
start_game(S, player, player):- make_board(S, B),
		game_loop(game(B, 0, 0, w), S).

/**
* add_captures(+P, +Captures, -Np)
* 	Adds a given number (Captures) to the given player captures (P)
*/
add_captures(P, Captures, Np):- Np is P + Captures.

/**
* game_loop(+game(B, Pw, Pb, Next))
* 	Next iteration of the game 
*/
game_loop(game(B, Pw, Pb, w), Size):- display_game(B, Pw, Pb, w),
		read_position(Row, Col),
		rep_piece_at(B, Row, Col, E),
		E == c, !,
		rep_internal(Size, [Row, Col], [RowI, ColI]),

		%	place_stone(w, B, [RowI, ColI], NewBoard, Captures),
		%	add_captures(Pw, Captures, Npw),

		move([RowI, ColI], game(B, Pw, Pb, w), game(NewBoard, Npw, _, Next),
		game_loop_aux(game(NewBoard, Npw, Pb, Next), Size).

game_loop(game(B, Pw, Pb, b), Size):- display_game(B, Pw, Pb, b),
		read_position(Row, Col),
		rep_piece_at(B, Row, Col, E),
		E == c, !,
		rep_internal(Size, [Row, Col], [RowI, ColI]),

		%	place_stone(b, B, [RowI, ColI], NewBoard, Captures),
		% add_captures(Pb, Captures, Npb),

		move([RowI, ColI], game(B, Pw, Pb, b), game(NewBoard, _, Npb, Next),
		game_loop_aux(game(NewBoard, Pw, Npb, Next), Size).

game_loop_aux(game(B, Pw, Pb, _), _Size):-	
	  game_over(game(B, Pw, Pb, _), w), !,
		display_game(B, Pw, Pb, w),
		victory(w).

game_loop_aux(game(B, Pw, Pb, _), _Size):-	
	  game_over(game(B, Pw, Pb, _), b), !,
		display_game(B, Pw, Pb, b),
		victory(b).

game_loop_aux(game(B, Pw, Pb, Next), Size):- game_loop(game(B, Pw, Pb, Next), Size).

/**
* victory(P)
* displays a victory message for the player P (w or b)
*/
victory(w):- write('White player won!'), nl.
victory(b):- write('Black player won!'), nl.

s_g(S):- start_game(S, player, player).
