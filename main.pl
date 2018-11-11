% Libraries

% Generating integers
:- use_module(library(between)).
% Basic operations on lists
:- use_module(library(lists)).

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

% Shorthands
re :- reconsult('main.pl').
test :- reconsult('test/test.pl').
retest :- re, test.
