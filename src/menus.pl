/**
	menu
	Displays the menus
*/

header :- nl,
	write('======================================'), nl,
	write('=                PENTE               ='), nl,
	write('======================================'), nl, nl.

footer :- nl,
	write('======================================'), nl,
	write('= Amadeu Pereira      Bruno Carvalho ='), nl,
	write('======================================'), nl, nl.

help_menu :-
	header,
	write('  To start playing just type play.'),nl,
	write('  It is possible for you to customize the game that you will play'),nl,
	write(' by setting up some values when starting it.'),nl,
	write('  You can implement these changes by starting the game as'),nl,
	write(' play(Options), where Options is a list of compounds.'),nl,nl,
	write('  Supported options: '),nl,nl,
	write('    - board_size(S), S = 1,3,5,...'),nl,
	write('        Board size (SxS), must be even!'),nl,nl,
	write('    - difficulty(Diff), Diff = 1,2,3,4,5'),nl,
	write('        Sets the bot difficulty (with 5 being the most difficult).'),nl,nl,
	write('    - depth(D), D = 1,2,3,...'),nl,
	write('        Bot s analyzation tree maximum depth.'),nl,nl,
	write('    - adding(P), P = 0,1,2,...'),nl,
	write('        Padding of the board on new analysis recursion.'),nl,nl,
	write('    - width([W1,W2,...]), Wi = 1,2,... for each i = 1,...,D'),nl,
	write('        For each depth i (top is depth 1) keep the Wi best children.'),nl,
	write('        If the width is not defined for all the depths, the last width'),nl,
	write('       defined will be used for those.'),nl,nl,
	write('    - width(W), W = 1,2,...'),nl,
	write('       Uses the width W for all depths.'),nl,nl,
	write('* When choosing a difficulty a predetermined configuration'),nl,
	write(' for the depth, padding and width values will be used.'),nl,
	write(' Although if the difficulty option is given and any of'),nl,
	write(' these values (depth, padding, width), these will override the'),nl,
	write(' difficulty default configuration.'),nl,nl,
	write('* If no options are given default ones will be used.'),nl,
	footer,
	write(' '), get_code(_).

main_menu(Options) :- 
	header,
	write('\t1- P1 vs P2'), nl,
	write('\t2- P1 vs BOT'), nl,
	write('\t3- BOT1 vs BOT2'), nl,nl,
	write('\t0- Exit'), nl,
	footer,
	get_input(Input),
	handle_input(Input, Options).

get_input(Input) :-
	write('> Option: '), nl,
	read(Input).

handle_input(1, Options) :- write('P1 vs P2'), nl, write(Options).
handle_input(2, Options) :- write('P1 vs BOT'), nl, write(Options).
handle_input(3, Options) :- write('BOT1 vs BOT2'), nl, write(Options).
handle_input(0, _) :- write('Exit').
handle_input(_, Options) :-
	write('Invalid Input'), nl,
	get_input(Input),
	handle_input(Input, Options).