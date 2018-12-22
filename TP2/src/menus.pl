/**
	menu
	Displays the menus
*/

header :-
	write('\e[2j'),
	write('================================================================'),nl,
	write('=                             Doors                            ='),nl,
	write('================================================================'),nl.

footer :-
	write('================================================================'),nl,
	write('= Amadeu Pereira                                Bruno Carvalho ='),nl,
	write('================================================================'),nl, nl.

main_menu :-
  header,
  write('\t\t1- Generate Random Board'), nl,
  write('\t\t2- Write Board'), nl, nl,
  write('\t\t0- Exit'), nl,
  footer,
  get_input(Input),
  handle_input(Input).

get_input(Input) :-
  write('> Option: '), nl,
  read(Input).

handle_input(1) :- !, random_board.
handle_input(2) :- !, write('Board: '), nl, read(Board), doors_calculator(Board).
handle_input(0) :- write('Exit').
handle_input(_) :-
  write('Invalid Input'), nl,
  get_input(Input),
  handle_input(Input).

random_board :-
  header,
  write('Rows: '), nl, read(R), integer(R),
  write('Cols: '), nl, read(C), integer(C), nl,
  generate_random_board([R,C], Board),
  write(Board), nl, nl,
  write('\t\t1- Start'), nl,
  write('\t\t2- New Board'), nl, nl,
  get_input(Input),
  handle_random_board_input(Input, Board).

handle_random_board_input(1, Board) :- !, doors_calculator(Board).
handle_random_board_input(2, _) :- !, random_board.
handle_random_board_input(_, Board) :-
  write('Invalid Input'), nl,
  get_input(Input),
  handle_random_board_input(Input, Board).
