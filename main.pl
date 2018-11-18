% ***** Libraries

% Generating integers
:- use_module(library(between)).
% Basic operations on lists
:- use_module(library(lists)).
% Random
:- use_module(library(random)).
% Sleep
:- use_module(library(system)).

% ***** User

% Library extensions
:- reconsult('src/general.pl').
:- reconsult('src/lists.pl').
:- reconsult('src/matrix.pl').
:- reconsult('src/random.pl').

% Board mini lib
:- reconsult('src/board.pl').

% Board display
:- reconsult('src/strings.pl').
:- reconsult('src/print-board.pl').
:- reconsult('src/help.pl').

% Player Bot
:- reconsult('src/score.pl').
:- reconsult('src/value.pl').
:- reconsult('src/tree.pl').

% Options
:- reconsult('src/options.pl').

% Game logic
:- reconsult('src/game.pl').
:- reconsult('src/input.pl').
:- reconsult('src/menus.pl').

% Examples
:- reconsult('src/examples.pl').

% ***** Shorthands

re :- reconsult('main.pl').
te :- reconsult('test/test.pl').

play :- pente.
play(Options) :- pente(Options).
help :- help_menu.
