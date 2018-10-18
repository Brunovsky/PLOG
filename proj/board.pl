% WHITE CIRCLE:       9675 u+25cb
% BLACK CIRCLE:       9679 u+25cf
% LARGE WHITE CIRCLE:      u+2b24
% LARGE BLACK CIRCLE:      u+25ef

% WHITE                    u+26aa 
% BLACK                    u+26ab

% Circled A-Z:        9398 u+24b6 - 9423 u+24cf
% Circled 1-20:       9312 u+2460 - 9331 u+2473

% HORIZONTAL DASH:    9472 u+2500
% VERTICAL DASH:      9474 u+2502
% http://jrgraphix.net/r/Unicode/2500-257F

% Other circles around these unicode codes.

print_codes(_, 0).
print_codes(X, N) :- M is N-1, K is X+1, put_code(X), put_char(' '), print_codes(K, M).

ok(X) :- format('~s~n\x2500\\x25ef\ \x2500\~n~s~n', [X, X]).

piece(w, _, '\x25cb\').
piece(b, _, '\x25cf\').
piece(' ', 1, '\x250c\').
piece(' ', 2, '\x252c\').
piece(' ', 3, '\x2510\').
piece(' ', 4, '\x251c\').
piece(' ', 5, '\x253c\').
piece(' ', 6, '\x2524\').
piece(' ', 7, '\x2514\').
piece(' ', 8, '\x2534\').
piece(' ', 9, '\x2518\').
piece(' ', _, '\x2500\').
% piece(' ', ' ').
% piece(s, '+').
% piece(S, S).

write_line([], _) :- write('\n').
write_line([H | T], O) :- 
    piece(H, O, R), write(R), write('\x2500\'),
    Q is O+1,
    write_line(T, Q).

write_board([], _).
write_board([L | Z], C) :- write_line(L, C), T is C+3, write_board(Z, T).

% mid(C) :- write_board([[w,b,b],[b,C,w],[b,w,b]]).
test :- write_board([[' ',w,' '],[b,' ',' '],[w,' ',' ']], 1).


% reconsult('board.pl').