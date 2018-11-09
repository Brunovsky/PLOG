/**
 * read_position(Row, Col).
 *   Read a board position from standard input.
 *   The position must be in the form E12, A7, B8, etc,
 *   and terminated with a dot.
 *   e.g. E12.
 *        A14.
 *        Z9.
 */
read_position(Row, Col) :- untilloop(is_alpha, get_char, Col),
                           peek_char(I),
                           (is_numeric(I),
                            read(Row),
                            (integer(Row);
                             \+ integer(Row),
                             read_position(Row, Col));
                            read_position(Row, Col)).
