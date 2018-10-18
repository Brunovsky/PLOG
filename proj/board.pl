% WHITE CIRCLE:       9675 u+25cb
% BLACK CIRCLE:       9679 u+25cf
% LARGE WHITE CIRCLE:      u+2b24
% LARGE BLACK CIRCLE:      u+25ef

% Circled A-Z:        9398 u+24b6 - 9423 u+24cf
% Circled 1-20:       9312 u+2460 - 9331 u+2473

% HORIZONTAL DASH:    9472 u+2500
% VERTICAL DASH:      9474 u+2502

% Other circles around these unicode codes.

print_codes(_, 0).
print_codes(X, N) :- M is N-1, K is X+1, put_code(X), put_char(' '), print_codes(K, M).

ok(X) :- format('~s~n\x2500\\x25ef\ \x2500\~n~s~n', [X, X]).

piece(w, '\x2b24\').
piece(b, '\x25ef\').
piece(' ', ' ').
piece(s, '+').
piece(S, S).

write_line([]) :- write('\n').
write_line([H | T]) :- piece(H, R), write(R), write(' '), write_line(T).

write_board([]).
write_board([L | Z]) :- write_line(L), write_board(Z).

write_mid(C) :- write_board([[w,b,b],[b,C,w],[b,w,b]]).
