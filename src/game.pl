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
