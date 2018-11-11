/**
	menu
	Displays the main menu
**/
menu :- 
	nl,
	write('\tPENTE'), nl, nl,
	write('1- P1 vs P2'), nl,
	write('2- P1 vs BOT'), nl,
	write('3- BOT1 vs BOT2'), nl,
	nl,
	write('0- Quit'),
	nl, nl,
	write('Amadeu Pereira\tBruno Carvalho'),
	nl,
	getMenuInput.

getMenuInput :-
	write('> Option: '),
	read(Input),
	handleMenuInput(Input).

handleMenuInput(1) :-
	start_game(19, player, player).

handleMenuInput(2) :-
	write('P1 vs BOT').

handleMenuInput(3) :-
	write('BOT1 vs BOT2').

handleMenuInput(0) :-
	write('Exit').

handleMenuInput(_) :-
	write('Invalid Option!'), nl,
	getMenuInput.
