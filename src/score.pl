/**
 * ===== ===== ===== ===== LIST EVALUATIONS ===== ===== ===== =====
 *
 * To evaluate the board, we will split it into rows, columns and diagonals
 * and evalaute these independently. Each will be called simply a 'list'.
 *
 * Each list is an ordered sequence of {c,w,b} stones. These stones form
 * patterns, which are segments of the list. For example, the list
 *                            [c,b,w,c,c,w,w,c,w,b]
 *                                     ^
 * has patterns [w,w,c,w], [c,c,w], etc, but not [w,w,w] or [b,w,w].
 * This list is good for White: if White plays in the caret position, it becomes
 * a win in 1 (five-in-a-row), regardless of where Black plays. So scoring this
 * list should favor White.
 *
 * We will score patterns independently in favor of White with integer points in
 * the range ]-∞,+∞[, as follows:
 * A winning pattern [w,w,w,w,w] will be worth pretty much +∞, say 2^90.
 * Strong patterns with forcing moves like [c,w,w,w,c,c], threating to make a
 * four-in-a-row (win in 1), will be scored highly, say 2^50 points. The actual
 * pre-win positions like [c,w,w,w,w,c] will be scored even higher, say 2^80 points.
 * Equivalent positions but for Black will be worth the opposite. So position
 * [c,b,b,b,b,c] would be worth -2^80 points.
 *
 * Notice that the list [c,b,b,b,b,c] encloses many patterns, such as [b,b,b],
 * [b,b] and [c,b,b]. Each of these patterns has their own score, which be added up
 * to the score of the pattern [c,b,b,b,b,c] itself to find the value of the list
 * [c,b,b,b,b,c].
 *
 * So we start by scoring patterns comprising only white pieces and empty positions
 * (pure patterns), then consider their reversals (replacing white with black pieces)
 */

/**
 * write_board_line_crude/1
 * write_board_line_crude(+L).
 */
write_board_line_crude(L) :-
    length(L, ColSize),
    ColSize2 is ColSize + 2,
    lb_foreach_increasing(L, write_board_unit, [[3,ColSize2], 2], 2).

/**
 * print_pattern_values/1
 * print_pattern_values(+Pattern).
 */
print_pattern_scores(Pattern, [PadLeft,PadRight]) :-
    numlist(0, PadLeft, LeftsReversed),
    numlist(0, PadRight, RightsReversed),
    reverse(LeftsReversed, Lefts), reverse(RightsReversed, Rights),
    (   foreach(Left, Lefts),
        param(Pattern),
        param(PadLeft),
        param(PadRight),
        param(Rights)
    do  (   foreach(Right, Rights),
            param(Pattern),
            param(PadLeft),
            param(PadRight),
            param(Left)
        do  fill_n(Left, c, CLeft),
            fill_n(Right, c, CRight),
            append([CLeft, Pattern, CRight], FullPattern),
            evaluate(FullPattern, Value),

            LeftSpaces is 2 * (PadLeft - Left),
            RightSpaces is 2 * (PadRight - Right),
            

            pretty_print_pattern(FullPattern, [LeftSpaces, RightSpaces]),
            format(' -- ~d~n', Value)
        )
    ).

/**
 * pretty_print_pattern/2
 * pretty_print_pattern(+Pattern, +Length).
 */
pretty_print_pattern(Pattern, [LeftSpaces, RightSpaces]) :-
    fill_n(LeftSpaces, ' ', LeftSpacesList),
    atom_chars(LeftString, LeftSpacesList),
    fill_n(RightSpaces, ' ', RightSpacesList),
    atom_chars(RightString, RightSpacesList),
    write(LeftString),
    write_board_line_crude(Pattern),
    write(RightString).

pretty_print_pattern(Pattern, Length) :-
    integer(Length), !,
    length(Pattern, PatternLength),
    FillSpaces is Length - PatternLength,
    fill_n(FillSpaces, ' ', SpacesList),
    atom_chars(SpaceString, SpacesList),
    write(SpaceString),
    write_board_line_crude(Pattern),
    write(SpaceString).

p(Pattern, Pad) :- print_pattern_scores(Pattern, Pad).

/**
 * pattern/2
 * pattern(+Pattern, -Score).
 *   Determines the score of a given pattern.
 */

/**
 * Pure Pattern with 1 stone.
 * Multiplier 2 ** 0
 * 9
 */

% - - w - - CHECK
pattern([c,c,w,c,c], 12).
pattern([c,c,w,c], 4).
pattern([c,c,w], 3).

pattern([c,w,c,c], 4).
pattern([c,w,c], 8).
pattern([c,w], 2).

pattern([w,c,c], 3).
pattern([w,c], 2).
pattern([w], 2).

/**
 * Pure Pattern with 2 stones.
 * Multiplier 2 ** 5
 * 31
 */

% - - w w - - CHECK
pattern([c,c,w,w,c,c], 16 * 2 ** 5).
pattern([c,c,w,w,c], 11 * 2 ** 5).
pattern([c,c,w,w], 16 * 2 ** 5).

pattern([c,w,w,c,c], 11 * 2 ** 5).
pattern([c,w,w,c], 64 * 2 ** 5).
pattern([c,w,w], -24 * 2 ** 5).

pattern([w,w,c,c], 16 * 2 ** 5).
pattern([w,w,c], -24 * 2 ** 5).
pattern([w,w], 12 * 2 ** 5).

% - - w - w - - CHECK
pattern([c,c,w,c,w,c,c], 50 * 2 ** 10).
pattern([c,c,w,c,w,c], 70 * 2 ** 10).
pattern([c,c,w,c,w], 80 * 2 ** 10).

pattern([c,w,c,w,c,c], 70 * 2 ** 10).
pattern([c,w,c,w,c], 130 * 2 ** 10).
pattern([c,w,c,w], 100 * 2 ** 10).

pattern([w,c,w,c,c], 80 * 2 ** 10).
pattern([w,c,w,c], 100 * 2 ** 10).
pattern([w,c,w], 250 * 2 ** 10).

% - - w - - w - - CHECK
pattern([c,c,w,c,c,w,c,c], 10 * 2 ** 8).
pattern([c,c,w,c,c,w,c], 10 * 2 ** 8).
pattern([c,c,w,c,c,w], 20 * 2 ** 8).

pattern([c,w,c,c,w,c,c], 10 * 2 ** 8).
pattern([c,w,c,c,w,c], 90 * 2 ** 8).
pattern([c,w,c,c,w], 90 * 2 ** 8).

pattern([w,c,c,w,c,c], 20 * 2 ** 8).
pattern([w,c,c,w,c], 90 * 2 ** 8).
pattern([w,c,c,w], 150 * 2 ** 8).

% - w - - - w - CHECK
pattern([c,w,c,c,c,w,c], 4 * 2 ** 5).
pattern([c,w,c,c,c,w], 3 * 2 ** 5).

pattern([w,c,c,c,w,c], 3 * 2 ** 5).
pattern([w,c,c,c,w], 5 * 2 ** 5).

/**
 * Pure Pattern with 3 stones.
 * Multiplier 2 ** 10
 * 40
 */

% - - w w w - - CHECK
pattern([c,c,w,w,w,c,c], 800 * 2 ** 28). % SENTE 2
pattern([c,c,w,w,w,c], 250 * 2 ** 26). % SENTE
pattern([c,c,w,w,w], 20 * 2 ** 24).

pattern([c,w,w,w,c,c], 250 * 2 ** 26). % SENTE
pattern([c,w,w,w,c], 15 * 2 ** 24).
pattern([c,w,w,w], 2 * 2 ** 24).

pattern([w,w,w,c,c], 20 * 2 ** 24).
pattern([w,w,w,c], 2 * 2 ** 24).
pattern([w,w,w], 8 * 2 ** 24).

% - w w - w -   @   - w - w w - CHECK
pattern([c,w,w,c,w,c], 34 * 2 ** 22). % SENTE
pattern([c,w,w,c,w], 48 * 2 ** 22).

pattern([w,w,c,w,c], 1 * 2 ** 22).
pattern([w,w,c,w], -4 * 2 ** 22).

pattern([c,w,c,w,w,c], 32 * 2 ** 22). % SENTE
pattern([c,w,c,w,w], 1 * 2 ** 22).

pattern([w,c,w,w,c], 48 * 2 ** 22).
pattern([w,c,w,w], -4 * 2 ** 22).

% - - w - w - w - - CHECK
pattern([c,c,w,c,w,c,w,c,c], 5 * 2 ** 16).
pattern([c,c,w,c,w,c,w,c], 7 * 2 ** 16).
pattern([c,c,w,c,w,c,w], 4 * 2 ** 16).

pattern([c,w,c,w,c,w,c,c], 7 * 2 ** 16).
pattern([c,w,c,w,c,w,c], 18 * 2 ** 16).
pattern([c,w,c,w,c,w], 8 * 2 ** 16).

pattern([w,c,w,c,w,c,c], 4 * 2 ** 16).
pattern([w,c,w,c,w,c], 8 * 2 ** 16).
pattern([w,c,w,c,w], 4 * 2 ** 16).

% - w - - w - w - -   @   - - w - w - - w - CHECK
pattern([c,w,c,c,w,c,w,c,c], 1 * 2 ** 11).
pattern([c,w,c,c,w,c,w,c], 1.5 * 2 ** 11).
pattern([c,w,c,c,w,c,w], 3 * 2 ** 11).

pattern([w,c,c,w,c,w,c,c], 1 * 2 ** 11).
pattern([w,c,c,w,c,w,c], 5 * 2 ** 11).
pattern([w,c,c,w,c,w], 12 * 2 ** 11).

pattern([c,c,w,c,w,c,c,w,c], 1 * 2 ** 11).
pattern([c,c,w,c,w,c,c,w], 1 * 2 ** 11).

pattern([c,w,c,w,c,c,w,c], 1.5 * 2 ** 11).
pattern([c,w,c,w,c,c,w], 5 * 2 ** 11).

pattern([w,c,w,c,c,w,c], 3 * 2 ** 11).
pattern([w,c,w,c,c,w], 12 * 2 ** 11).

/**
 * Pure Pattern with 4 stones.
 * 2 ** 30
 * 27
 */

% - w w w w - CHECK
pattern([c,w,w,w,w,c], 2 ** 65). % WIN IN 1.
pattern([c,w,w,w,w], 2 ** 38). % SENTE

pattern([w,w,w,w,c], 2 ** 38). % SENTE
pattern([w,w,w,w], 3 * 2 ** 25).

% - - w w w - w   @   w - w w w - - CHECK
pattern([c,c,w,w,w,c,w], 2 ** 39). % SENTE 2
pattern([c,w,w,w,c,w], 2 ** 30). % SENTE
pattern([w,w,w,c,w], 2 ** 38). % SENTE

pattern([w,c,w,w,w,c,c], 2 ** 42). % SENTE 2
pattern([w,c,w,w,w,c], 2 ** 37). % SENTE
pattern([w,c,w,w,w], 2 ** 40). % SENTE

% - w - w w - w - CHECK
pattern([c,w,c,w,w,c,w,c], 2 ** 43). % SENTE 2
pattern([c,w,c,w,w,c,w], 2 ** 39). % SENTE

pattern([w,c,w,w,c,w,c], 2 ** 39). % SENTE
pattern([w,c,w,w,c,w], 2 ** 18).

% - - w - w - w - w - - CHECK
pattern([c,c,w,c,w,c,w,c,w,c,c], 2 ** 28). % SENTE
pattern([c,c,w,c,w,c,w,c,w,c], 2 ** 28). % SENTE
pattern([c,c,w,c,w,c,w,c,w], 2 ** 32). % SENTE

pattern([c,w,c,w,c,w,c,w,c,c], 2 ** 28). % SENTE
pattern([c,w,c,w,c,w,c,w,c], 2 ** 31). % SENTE
pattern([c,w,c,w,c,w,c,w], 2 ** 27). % SENTE

pattern([w,c,w,c,w,c,w,c,c], 2 ** 32). % SENTE
pattern([w,c,w,c,w,c,w,c], 2 ** 27). % SENTE
pattern([w,c,w,c,w,c,w], 3 * 2 ** 38). % SENTE

% - w w - w w - CHECK
pattern([c,w,w,c,w,w,c], 2 ** 22).
pattern([c,w,w,c,w,w], 2 ** 21).

pattern([w,w,c,w,w,c], 2 ** 21).
pattern([w,w,c,w,w], -2 ** 24).

/**
 * Pure Pattern with 5 stones.
 * 2 ** 35
 * ! TODO
 * 29
 */

% w w w w w
pattern([w,w,w,w,w], 2 ** 100). % WIN.

% w - w w w - w
pattern([w,c,w,w,w,c,w], 2 ** 67). % WIN IN 1.

% - w w - w - w w -
pattern([c,w,w,c,w,c,w,w,c], 3 * 2 ** 40). % SENTE 2
pattern([c,w,w,c,w,c,w,w], 2 ** 38). % SENTE

pattern([w,w,c,w,c,w,w,c], 2 ** 38). % SENTE
pattern([w,w,c,w,c,w,w], 2 ** 30).

% - - w w w - w w   @   w w - w w w - -
pattern([c,c,w,w,w,c,w,w], 2 ** 39). % SENTE 2
pattern([c,w,w,w,c,w,w], 2 ** 34). % SENTE
pattern([w,w,w,c,w,w], 2 ** 34). % SENTE

pattern([w,w,c,w,w,w,c,c], 2 ** 39). % SENTE 2
pattern([w,w,c,w,w,w,c], 2 ** 34). % SENTE
pattern([w,w,c,w,w,w], 2 ** 34). % SENTE

% - w - w w - w w   @   w w - w w - w -
pattern([c,w,c,w,w,c,w,w], 3 * 2 ** 40). % SENTE 2
pattern([w,c,w,w,c,w,w], 2 ** 36). % SENTE

pattern([w,w,c,w,w,c,w,c], 3 * 2 ** 40). % SENTE 2
pattern([w,w,c,w,w,c,w], 2 ** 36). % SENTE

% - - w - w - w - w - w - -
pattern([c,c,w,c,w,c,w,c,w,c,w,c,c], 2 ** 45). % SENTE 3
pattern([c,c,w,c,w,c,w,c,w,c,w,c], 2 ** 39). % SENTE 2
pattern([c,c,w,c,w,c,w,c,w,c,w], 2 ** 39). % SENTE 2

pattern([c,w,c,w,c,w,c,w,c,w,c,c], 2 ** 39). % SENTE 2
pattern([c,w,c,w,c,w,c,w,c,w,c], 3 * 2 ** 36). % SENTE 2
pattern([c,w,c,w,c,w,c,w,c,w], 3 * 2 ** 36). % SENTE 2

pattern([w,c,w,c,w,c,w,c,w,c,c], 2 ** 39). % SENTE 2
pattern([w,c,w,c,w,c,w,c,w,c], 3 * 2 ** 36). % SENTE 2
pattern([w,c,w,c,w,c,w,c,w], 3 * 2 ** 36). % SENTE 2

% - w - w w - w - w   @   w - w - w w - w -
pattern([c,w,c,w,w,c,w,c,w], 2 ** 41). % SENTE 2
pattern([w,c,w,w,c,w,c,w], 2 ** 37). % SENTE

pattern([w,c,w,c,w,w,c,w,c], 2 ** 41). % SENTE 2
pattern([w,c,w,c,w,w,c,w], 2 ** 37). % SENTE

/**
 * Pure Pattern with 6 stones.
 * ! TODO
 * 14
 */

% - - w w w - w w w - -
pattern([c,c,w,w,w,c,w,w,w,c,c], 2 ** 47). % SENTE 3
pattern([c,c,w,w,w,c,w,w,w,c], 2 ** 41). % SENTE 2
pattern([c,c,w,w,w,c,w,w,w], 2 ** 41). % SENTE 2

pattern([c,w,w,w,c,w,w,w,c,c], 2 ** 41). % SENTE 2
pattern([c,w,w,w,c,w,w,w,c], 2 ** 33). % SENTE
pattern([c,w,w,w,c,w,w,w], 2 ** 33). % SENTE

pattern([w,w,w,c,w,w,w,c,c], 2 ** 41). % SENTE 2
pattern([w,w,w,c,w,w,w,c], 2 ** 33). % SENTE
pattern([w,w,w,c,w,w,w], 2 ** 33). % SENTE

% w w - w w - w w
pattern([w,w,c,w,w,c,w,w], 2 ** 66). % WIN IN 1.

% - w - w w - w w w - -
pattern([c,w,c,w,w,c,w,w,w,c,c], 3 * 2 ** 45). % SENTE 3
pattern([c,w,c,w,w,c,w,w,w,c], 2 ** 38). % SENTE 2
pattern([c,w,c,w,w,c,w,w,w], 2 ** 40). % SENTE 2

pattern([w,c,w,w,c,w,w,w,c,c], 2 ** 36). % SENTE 2
pattern([w,c,w,w,c,w,w,w,c], 2 ** 32). % SENTE
pattern([w,c,w,w,c,w,w,w], 2 ** 32). % SENTE

pattern([c,c,w,w,w,c,w,w,c,w,c], 3 * 2 ** 45). % SENTE 3
pattern([c,c,w,w,w,c,w,w,c,w], 2 ** 36). % SENTE 2

pattern([c,w,w,w,c,w,w,c,w,c], 2 ** 38). % SENTE 2
pattern([c,w,w,w,c,w,w,c,w], 2 ** 32). % SENTE

pattern([w,w,w,c,w,w,c,w,c], 2 ** 40). % SENTE 2
pattern([w,w,w,c,w,w,c,w], 2 ** 32). % SENTE

/**
 * dynamic score/2
 * score(+Pattern, -Score).
 *   Uses pattern/2 to score a given pattern for white or black.
 */
 :- abolish(score/2). % reload
 :- findall([Pattern, Score], pattern(Pattern, Score), List),
    (   foreach([WPattern, Score], List)
    do  (   list_reversal(WPattern, BPattern),
            BScore is -Score,
            WScore is Score,
            assertz((score(WPattern, WScore))),
            assertz((score(BPattern, BScore)))
        )
    ).

/**
 * dynamic score_list/1
 * score_list(-PatternList).
 *   Get the list of scored patterns.
 */
 :- abolish(score_list/1). % reload
 :- findall(Pattern, score(Pattern, _), PatternList),
    asserta((score_list(PatternList) :- !)).

/**
 * multiscore/3
 * multiscore(+List, +Pattern, -Score).
 *   Find all instances of Pattern in List, and accumulate their score.
 */
multiscore(List, Pattern, TotalScore) :-
    score(Pattern, Score),
    countsegment(List, Pattern, N),
    TotalScore is Score * N.

/**
 * captures_score/4
 * captures_score(Wc, Bc, Score).
 *   Setting a score to a pair of captures (Wc,Bc).
 */
captures_score([C,C], 0) :- 0 is mod(C, 2), C < 10, C >= 0.

captures_score([2,0], 2 ** 25).
captures_score([4,0], 2 ** 37).
captures_score([6,0], 2 ** 52).
captures_score([8,0], 2 ** 77).
captures_score([4,2], 2 ** 20).
captures_score([6,2], 2 ** 49).
captures_score([8,2], 2 ** 76).
captures_score([6,4], 2 ** 44).
captures_score([8,4], 2 ** 74).
captures_score([8,6], 2 ** 69).
captures_score([10,_], 2 ** 90).

captures_score([Wc,Bc], Score) :-
    Wc < Bc,
    captures_score([Bc,Wc], WScore),
    Score is -WScore.

/**
 * dynamic evaluate/2
 * evaluate(+List, -Value).
 *   Evaluate a list.
 */
 :- abolish(evaluate/2). % reload
 :- dynamic evaluate/2.

evaluate(List, Value) :-
    score_list(Patterns),
    map(multiscore(List), Patterns, Scores), !, % very expensive
    scanlist(plus, Scores, 0, Value),
    asserta((evaluate(List, Value))), !. % store for future calls on the same list.

check_evaluate(List) :-
    length(List, Length),
    count_element(w, List, Whites),
    score_list(Patterns),
    (   foreach(Pattern, Patterns),
        param(List),
        param(Length),
        param(Whites)
    do  score(Pattern, Score),
        countsegment(List, Pattern, N),
        count_element(w, Pattern, PWhites),
        (N > 0, PWhites = Whites ->
            pretty_print_pattern(Pattern, Length),
            evaluate(Pattern, V),
            format('~t  --~t~6| ~d~t~6+| ~D~t~20+| ~D~n', [N, Score, V]);
            true
        )
    ),
    evaluate(List, Value),
    format('Total Value: ~d~n', Value).

check_evaluate_all(List) :-
    length(List, Length),
    score_list(Patterns),
    (   foreach(Pattern, Patterns),
        param(List),
        param(Length)
    do  score(Pattern, Score),
        countsegment(List, Pattern, N),
        (N > 0 ->
            pretty_print_pattern(Pattern, Length),
            evaluate(Pattern, V),
            format('~t  --~t~6| ~d~t~6+| ~D~t~20+| ~D~n', [N, Score, V]);
            true
        )
    ),
    evaluate(List, Value),
    format('Total Value: ~D~n', Value).

e(List) :- check_evaluate(List).
a(List) :- check_evaluate_all(List).