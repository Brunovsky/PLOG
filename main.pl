% ***** Libraries

% Generating integers
:- use_module(library(between)).
% Basic operations on lists
:- use_module(library(lists)).
% Random
:- use_module(library(random)).

% ***** User

% Library extensions
:- compile('src/general.pl').
:- compile('src/lists.pl').
:- compile('src/matrix.pl').
:- compile('src/random.pl').

% Board mini lib
:- compile('src/board.pl').

% Board display
:- compile('src/strings.pl').
:- compile('src/print-board.pl').

% Player Bot
:- compile('src/score.pl').
:- compile('src/value.pl').
:- compile('src/tree-opt.pl').
:- compile('src/tree.pl').

% Game logic
:- compile('src/game.pl').
:- compile('src/input.pl').
:- compile('src/menus.pl').

% Examples
:- compile('src/examples.pl').

% ***** Shorthands

re :- compile('main.pl').
te :- compile('test/test.pl').
