/**
 * Game from https://www.youtube.com/watch?v=lNFbF1_eZLQ
 */
plog(earlygame) :- display_game([
%    A  B  C  D  E  F  G  H  J  K  L  M  N  O  P  Q  R  S  T
    [c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c], % 19
    [c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c], % 18
    [c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c], % 17
    [c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c], % 16
    [c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c], % 15
    [c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c], % 14
    [c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c], % 13
    [c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c], % 12
    [c, c, c, c, c, c, c, c, b, c, b, c, c, c, c, c, c, c, c], % 11
    [c, c, c, c, c, c, c, c, c, w, w, b, w, c, c, c, c, c, c], % 10
    [c, c, c, c, c, c, c, c, c, b, c, w, c, c, c, c, c, c, c], % 9
    [c, c, c, c, c, c, c, c, c, c, w, c, c, c, c, c, c, c, c], % 8
    [c, c, c, c, c, c, c, c, c, b, c, c, c, c, c, c, c, c, c], % 7
    [c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c], % 6
    [c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c], % 5
    [c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c], % 4
    [c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c], % 3
    [c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c], % 2
    [c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c]  % 1
], player(white, 0), player(black, 0), w).

plog(midgame) :- display_game([
%    A  B  C  D  E  F  G  H  J  K  L  M  N  O  P  Q  R  S  T
    [c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c], % 19
    [c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c], % 18
    [c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c], % 17
    [c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c], % 16
    [c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c], % 15
    [c, c, c, c, c, c, c, c, c, b, b, c, c, b, c, c, c, c, c], % 14
    [c, c, c, c, c, c, c, c, b, c, w, c, w, c, c, c, c, c, c], % 13
    [c, c, c, c, c, c, c, c, c, c, c, w, b, c, c, c, c, c, c], % 12
    [c, c, c, c, c, c, c, c, b, b, w, c, w, c, c, c, c, c, c], % 11
    [c, c, c, c, c, c, c, c, c, w, c, b, w, c, c, c, c, c, c], % 10
    [c, c, c, c, c, c, c, c, b, b, c, w, w, b, c, c, c, c, c], % 9
    [c, c, c, c, c, c, c, c, c, c, w, c, b, c, c, c, c, c, c], % 8
    [c, c, c, c, c, c, c, c, c, b, c, c, c, c, c, c, c, c, c], % 7
    [c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c], % 6
    [c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c], % 5
    [c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c], % 4
    [c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c], % 3
    [c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c], % 2
    [c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c]  % 1
], player(white, 4), player(black, 2), b). 

plog(lategame) :- display_game([
%    A  B  C  D  E  F  G  H  J  K  L  M  N  O  P  Q  R  S  T
    [c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c], % 19
    [c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c], % 18
    [c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c], % 17
    [c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c], % 16
    [c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c], % 15
    [c, c, c, c, c, c, c, c, c, b, c, c, c, b, c, c, c, c, c], % 14
    [c, c, c, c, c, c, c, c, b, c, w, c, w, c, c, c, c, c, c], % 13
    [c, c, c, c, c, c, c, c, b, c, c, w, b, c, c, b, c, c, c], % 12
    [c, c, c, c, c, c, c, c, b, b, w, c, w, c, w, c, c, c, c], % 11
    [c, c, c, c, c, c, c, c, w, w, c, b, c, w, c, c, c, c, c], % 10
    [c, c, c, c, c, c, c, c, b, b, c, w, c, c, b, w, c, c, c], % 9
    [c, c, c, c, c, c, c, c, c, c, w, c, c, c, w, c, c, c, c], % 8
    [c, c, c, c, c, c, c, c, c, b, w, c, c, c, b, c, c, c, c], % 7
    [c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c], % 6
    [c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c], % 5
    [c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c], % 4
    [c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c], % 3
    [c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c], % 2
    [c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c]  % 1
], player(white, 8), player(black, 8), w).

plog(early13) :- display_game([
%    A  B  C  D  E  F  G  H  J  K  L  M  N
    [c, c, c, c, c, c, c, c, c, c, c, c, c], % 13
    [c, c, c, c, c, c, c, c, c, c, c, c, c], % 12
    [c, c, c, c, c, c, c, c, c, c, c, c, c], % 11
    [c, c, c, c, c, c, c, c, w, c, c, c, c], % 10
    [c, c, c, c, c, c, c, c, c, c, c, c, c], % 9
    [c, c, c, c, c, c, b, c, w, c, c, c, c], % 8
    [c, c, c, c, c, c, w, w, b, b, w, c, c], % 7
    [c, c, c, c, c, c, c, c, c, w, c, c, c], % 6
    [c, c, c, c, c, c, c, w, b, c, c, c, c], % 5
    [c, c, c, c, c, c, c, c, c, c, c, c, c], % 4
    [c, c, c, c, c, c, c, c, c, c, c, c, c], % 3
    [c, c, c, c, c, c, c, c, c, c, c, c, c], % 2
    [c, c, c, c, c, c, c, c, c, c, c, c, c]  % 1
], player(white, 0), player(black, 0), w).

plog_at(Row, Col, E) :- piece_at([
%    A  B  C  D  E  F  G  H  J  K  L  M  N  O  P  Q  R  S  T
    [c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c], % 19
    [c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c], % 18
    [c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c], % 17
    [c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c], % 16
    [c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c], % 15
    [c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c], % 14
    [c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c], % 13
    [c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c], % 12
    [c, c, c, c, c, c, c, c, b, c, b, c, c, c, c, c, c, c, c], % 11
    [c, c, c, c, c, c, c, c, c, w, w, b, w, c, c, c, c, c, c], % 10
    [c, c, c, c, c, c, c, c, c, b, c, w, c, c, c, c, c, c, c], % 9
    [c, c, c, c, c, c, c, c, c, c, w, c, c, c, c, c, c, c, c], % 8
    [c, c, c, c, c, c, c, c, c, b, c, c, c, c, c, c, c, c, c], % 7
    [c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c], % 6
    [c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c], % 5
    [c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c], % 4
    [c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c], % 3
    [c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c], % 2
    [c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c]  % 1
], Row, Col, E).

plog_at(String, E) :- piece_at([
%    A  B  C  D  E  F  G  H  J  K  L  M  N  O  P  Q  R  S  T
    [c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c], % 19
    [c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c], % 18
    [c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c], % 17
    [c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c], % 16
    [c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c], % 15
    [c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c], % 14
    [c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c], % 13
    [c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c], % 12
    [c, c, c, c, c, c, c, c, b, c, b, c, c, c, c, c, c, c, c], % 11
    [c, c, c, c, c, c, c, c, c, w, w, b, w, c, c, c, c, c, c], % 10
    [c, c, c, c, c, c, c, c, c, b, c, w, c, c, c, c, c, c, c], % 9
    [c, c, c, c, c, c, c, c, c, c, w, c, c, c, c, c, c, c, c], % 8
    [c, c, c, c, c, c, c, c, c, b, c, c, c, c, c, c, c, c, c], % 7
    [c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c], % 6
    [c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c], % 5
    [c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c], % 4
    [c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c], % 3
    [c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c], % 2
    [c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c]  % 1
], String, E).
