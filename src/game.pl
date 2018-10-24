/**
 * player(Color, Captures).
 *   A player of Pente, with a color, a certain number of Captures and a series of plays.
 * Color: white or black.
 * Captures: An int (between 0 and 10).
 */
player(_, _).

/**
 * game(Board, White, Black, next).
 *   A game of Pente.
 *   > The current Board is represented by a 19x19 matrix, consisting of
 *     characters c for empty slots, w for White's pieces and b for Black's pieces.
 *   > White and Black are players.
 *   > next is w or b, indicating whose turn it is to play.
 */
game(_, _, _, _).
