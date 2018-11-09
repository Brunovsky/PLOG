% Everything in src/
:- reconsult('src/board.pl').
:- reconsult('src/examples.pl').
:- reconsult('src/game.pl').
:- reconsult('src/general.pl').
:- reconsult('src/lists.pl').
:- reconsult('src/matrix.pl').
:- reconsult('src/menus.pl').
:- reconsult('src/print-board.pl').
:- reconsult('src/strings.pl').

% Faster reconsult.
re :- reconsult('main.pl').

/******************************************
 *************** TEST SPACE ***************
 *****************************************/ 
read_position(Row, Col) :- untilloop(is_alpha, get_char, Col),
                           peek_char(I),
                           (is_numeric(I),
                            read(Row),
                            (integer(Row);
                             \+ integer(Row),
                             read_position(Row, Col));
                            read_position(Row, Col)).
