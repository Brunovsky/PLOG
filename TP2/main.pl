:- use_module(library(clpfd)).
:- use_module(library(lists)).
:- use_module(library(random)).

:- compile('src/doors.pl').
:- compile('src/matrix.pl').
:- compile('src/lists.pl').
:- compile('src/print.pl').
:- compile('src/statistics.pl').
:- compile('src/random.pl').
:- compile('src/menus.pl').

re :- compile('main.pl').
doors :- main_menu.
doors(Board) :- doors_calculator(Board).
