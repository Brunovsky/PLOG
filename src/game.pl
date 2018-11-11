/**
 * player(?Color, ?Captures).
 *   A player of Pente, with a color, a certain number of Captures
 *   and a series of plays.
 * Color: w or b.
 * Captures: An int (between 0 and 10).
 */
player(_, _).

/**
 * other_player(?P1, ?P2).
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
																game_loop(game(B, player(w, 0), player(b, 0), w)).

/**
* add_captures(+player(C, T), Captures, -Np)
* 	Adds a given number (Captures) to the given player
*/
add_captures(player(C, T), Captures, Np):- Nt is T + Captures, Np = player(C, Nt).

/**
* game_loop(+game(B, Pw, Pb, Next))
* 	Next iteration of the game 
*/
game_loop(game(B, Pw, Pb, w)):- display_game(B, Pw, Pb, w),
																read_position(Row, Col),
																%place_stone(w, [Row, Col], B, NewBoard, Captures),
																add_captures(Pw, 2, Npw),
																game_loop_aux(game(B, Pw, Pb, w), Npw).

game_loop(game(B, Pw, Pb, b)):- display_game(B, Pw, Pb, b),
																read_position(Row, Col),
																%place_stone(b, [Row, Col], B, NewBoard, Captures),
																add_captures(Pb, 2, Npb),
																game_loop_aux(game(B, Pw, Pb, b), Npb).

game_loop_aux(game(B, Pw, Pb, N), _):- other_player(N, P),
																		game_over(game(B, Pw, Pb, N), P),
															      write("Game Ended").

game_loop_aux(game(B, Pw, Pb, w), Npw):- game_loop(game(B, Npw, Pb, b)).

game_loop_aux(game(B, Pw, Pb, b), Npb):- game_loop(game(B, Pw, Npb, w)).
