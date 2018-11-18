% https://pente.org/gameServer/forums/thread.jspa?forumID=27&threadID=4553

test_linear_pure_two :-
    % 1 - - w - w - -
    evaluate([c,c,w,c,w,c,c], Stretch), StretchC is Stretch * 3,
    % 2 - - w - - w - -
    evaluate([c,c,w,c,c,w,c,c], StretchThree), StretchThreeC is StretchThree * (2 ** 4),
    % 3 - - w w - -
    evaluate([c,c,w,w,c,c], Pair), PairC is Pair * (2 ** 9),
    % 4 - - w - - - w - -
    evaluate([c,c,w,c,c,c,w,c,c], StretchFour), StretchFourC is StretchFour * (2 ** 11),
    % ASSERT:
    !, ordered(>, [StretchC, StretchThreeC, PairC, StretchFourC]).

test_linear_pure_three :-
    % - - w w w - -
    evaluate([c,c,w,w,w,c,c], Open), OpenC is Open,
    % - - w w - w - -
    evaluate([c,c,w,w,c,w,c,c], Stretch), StretchC is Stretch * (2 ** 3),
    % - - w - w - w - -
    evaluate([c,c,w,c,w,c,w,c,c], Split), SplitC is Split * (2 ** 8),
    % - - w - - w - w - -
    evaluate([c,c,w,c,c,w,c,w,c,c], SS), SSC is SS * (2 ** 10),
    % ASSERT:
    !, ordered(>, [OpenC, StretchC, SplitC, SSC]).

test_linear_pure_four :-
    % w - w - w - w
    evaluate([w,c,w,c,w,c,w], Scorpion), ScorpionC is Scorpion * 5,
    % - w - w w - w -
    evaluate([c,w,c,w,w,c,w,c], OutSplit), OutSplitC is OutSplit * (2 ** 3),
    % - - w w w - w
    evaluate([c,c,w,w,w,c,w], Stretch), StretchC is Stretch * (2 ** 3),
    % - w w w w
    evaluate([c,w,w,w,w], Open), OpenC is Open * (2 ** 6),
    % w w w w
    evaluate([w,w,w,w], Closed), ClosedC is Closed * (2 ** 15),
    % - w w - w w -
    evaluate([c,w,w,c,w,w,c], InSplit), InSplitC is InSplit * (2 ** 14),
    % ASSERT:
    !, ordered(>, [ScorpionC,OutSplitC,StretchC,OpenC,ClosedC,InSplitC]).

test_linear_pure_wininone :-
    % - w w w w -
    evaluate([c,w,w,w,w,c], FourRow), FourRowC is FourRow,
    % w - w w w - w
    evaluate([w,c,w,w,w,c,w], DStretchThree), DStretchThreeC is DStretchThree * (2 ** 3),
    % w w - w w - w w
    evaluate([w,w,c,w,w,c,w,w], TriplePair), TriplePairC is TriplePair * (2 ** 5),
    % ASSERT:
    !, ordered(>, [FourRowC,DStretchThreeC,TriplePairC]).

test_linear_pure_fewstones :-
    % - - w - w -
    evaluate([c,c,w,c,w,c], StretchTwo), StretchTwoC is StretchTwo * (2 ** 6),
    % - - w - w w -
    evaluate([c,c,w,c,w,w,c], StretchThree), StretchThreeC is StretchThree,
    % ASSERT:
    !, ordered(>, [StretchTwoC,StretchThreeC]).

sente_between([MultLower,MultUpper], A, B) :-
    LowerBound is A * MultLower,
    UpperBound is A * MultUpper,
    LowerBound =< B, B =< UpperBound.

sente2(A, B) :- sente_between([0.4,10.0], A, B).
sente3(A, B) :- sente_between([0.05,3.0], A, B).

test_sente :-
    % 3 Stones

    % - - w w w b -
    evaluate([c,c,w,w,w,c,c], A1),
    % - w w w w
    evaluate([c,w,w,w,w], B1), sente2(A1, B1), !,

    % 4 Stones
    
    % - - w w w b w
    evaluate([c,c,w,w,w,c,w], A2),
    % - w w w w
    evaluate([c,w,w,w,w], B2), sente2(A2, B2), !,

    % - w - w w b w -
    evaluate([c,w,c,w,w,c,w,c], A3),
    % - w w w w
    evaluate([c,w,w,w,w], B3), sente2(A3, B3), !,

    % - - w b w - w - w - -
    evaluate([c,c,w,c,w,c,w,c,w,c,c], A4),
    % w w w - w - -
    evaluate([w,w,w,c,w,c,c], B41), sente2(A4, B41), !,
    % w - w w w - -
    evaluate([w,c,w,w,w,c,c], B42), sente2(A4, B42), !,

    % 5 Stones

    % - w w - w b w w -
    evaluate([c,w,w,c,w,c,w,w,c], A5),
    % - w w w w
    evaluate([c,w,w,w,w], B5), sente2(A5, B5), !,

    % - - w w w b w w
    evaluate([c,c,w,w,w,c,w,w], A6),
    % - w w w w
    evaluate([c,w,w,w,w], B6), sente2(A6, B6), !,

    % - w - w w b w w w
    evaluate([c,w,c,w,w,c,w,w,w], A7),
    % - w w w w
    evaluate([c,w,w,w,w], B7), sente2(A7, B7), !,

    % - - w - w b w - w - w - -
    evaluate([c,c,w,c,w,c,w,c,w,c,w,c,c], A8),
    % w w w - w - -
    evaluate([w,w,w,c,w,c,c], B81), sente2(A8, B81), !,
    % w - w w w - -
    evaluate([w,c,w,w,w,c,c], B82), sente2(A8, B82), !,

    % - w - w w b w - w
    evaluate([c,w,c,w,w,c,w,c,w], A9),
    % - w w w w
    evaluate([c,w,w,w,w], B9), sente2(A9, B9), !,

    % 6 Stones

    % - - w w w - w w w - -
    evaluate([c,c,w,w,w,c,w,w,w,c,c], A10),
    % - w w w w
    evaluate([c,w,w,w,w], B10), sente3(A10, B10), !,

    % - w - w w b w w w - -
    evaluate([c,w,c,w,w,c,w,w,w,c,c], A11),
    % - w w w w
    evaluate([c,w,w,w,w], B11), sente2(A11, B11), !,

    % Children 5 Stones

    % w - w - w b w - w
    evaluate([w,c,w,c,w,c,w,c,w], A12),
    % w - w w w
    evaluate([w,c,w,w,w], B12), sente2(A12, B12), !,

    % - - w w w b w w w
    evaluate([c,c,w,w,w,c,w,w,w], A13),
    % - w w w w
    evaluate([c,w,w,w,w], B13), sente2(A13, B13), !,

    % - w - w w b w w w
    evaluate([c,w,c,w,w,c,w,w,w], A14),
    % - w w w w
    evaluate([c,w,w,w,w], B14), sente2(A14, B14), !,

    % w - w w b w w w - -
    evaluate([w,c,w,w,c,w,w,w,c,c], A15),
    % w w w w -
    evaluate([w,w,w,w,c], B15), sente2(A15, B15), !.


test_strategy :- test_all(strategy, [
    test_linear_pure_two,
    test_linear_pure_three,
    test_linear_pure_four,
    test_linear_pure_wininone,
    test_linear_pure_fewstones,
    test_sente
]).
