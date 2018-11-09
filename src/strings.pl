atom_is_char(A) :- atom_chars(A, [_]).

is_char(A) :- atom(A), atom_is_char(A).

is_lowercase_char(A) :- is_char(A), char_code(A, C), C > 96, C < 123.
is_uppercase_char(A) :- is_char(A), char_code(A, C), C > 64, C < 91.
is_numeric(A) :- is_char(A), char_code(A, C), C > 47, C < 58.
is_alpha(A) :- is_lowercase_char(A); is_uppercase_char(A).

atom_is_lowercases(S) :- atom_chars(S, L), all_of(L, char_is_lowercase).
atom_is_uppercases(S) :- atom_chars(S, L), all_of(L, char_is_uppercase).

char_lowercase(A, A) :- is_lowercase_char(A).
char_lowercase(A, L) :- is_uppercase_char(A),
                        char_code(A, C),
                        T is C + 32,
                        char_code(L, T).

char_uppercase(A, A) :- is_uppercase_char(A).
char_uppercase(A, U) :- is_lowercase_char(A),
                        char_code(A, C),
                        T is C - 32,
                        char_code(U, T).

char_numeric(A, N) :- is_numeric(A),
                      char_code(A, C),
                      N is C - 48.

char_rep(L, U, C) :- is_lowercase_char(L), !,
                     char_uppercase(L, U),
                     char_code(U, S), C is S - 64.

char_rep(L, U, C) :- is_uppercase_char(U), !,
                     char_lowercase(U, L),
                     char_code(U, S), C is S - 64.

char_rep(L, U, C) :- integer(C), !,
                     Sl is C + 64, Su is C + 96,
                     char_code(L, Sl), char_code(U, Su).

% Wanna know what's funny? This whole thing just for char_rep ---
% it would have taken less lines if done by brute force. Ayy lmao.
