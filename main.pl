% Libraries

% Generating integers
:- use_module(library(between)).
% Basic operations on lists
:- use_module(library(lists)).
% Random
:- use_module(library(random)).

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
:- reconsult('src/score.pl').
:- reconsult('src/strings.pl').
:- reconsult('src/tree-opt.pl').
:- reconsult('src/tree.pl').
:- reconsult('src/value.pl').
:- reconsult('src/random.pl').
:- reconsult('src/score.pl').

% Shorthands
re :- reconsult('main.pl').
te :- reconsult('test/test.pl').
rete :- re, te.
