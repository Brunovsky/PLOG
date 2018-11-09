% Everything in src/
:- reconsult('src/board.pl').
:- reconsult('src/examples.pl').
:- reconsult('src/game.pl').
:- reconsult('src/general.pl').
:- reconsult('src/input.pl').
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
