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
 * pattern/2
 * pattern(+Pattern, -Score).
 *   Determines the score of a given pattern.
 */

/**
 * Pure Pattern with 1 stone.
 * ! TODO
 * 9
 */

% - - w - -
pattern([c,c,w,c,c], 1).
pattern([c,c,w,c], 1).
pattern([c,c,w], 1).

pattern([c,w,c,c], 1).
pattern([c,w,c], 1).
pattern([c,w], 1).

pattern([w,c,c], 1).
pattern([w,c], 1).
pattern([w], 1).

/**
 * Pure Pattern with 2 stones.
 * ! TODO
 * 31
 */

% - - w w - -
pattern([c,c,w,w,c,c], 1).
pattern([c,c,w,w,c], 1).
pattern([c,c,w,w], 1).

pattern([c,w,w,c,c], 1).
pattern([c,w,w,c], 1).
pattern([c,w,w], 1).

pattern([w,w,c,c], 1).
pattern([w,w,c], 1).
pattern([w,w], 1).

% - - w - w - -
pattern([c,c,w,c,w,c,c], 1).
pattern([c,c,w,c,w,c], 1).
pattern([c,c,w,c,w], 1).

pattern([c,w,c,w,c,c], 1).
pattern([c,w,c,w,c], 1).
pattern([c,w,c,w], 1).

pattern([w,c,w,c,c], 1).
pattern([w,c,w,c], 1).
pattern([w,c,w], 1).

% - - w - - w - -
pattern([c,c,w,c,c,w,c,c], 1).
pattern([c,c,w,c,c,w,c], 1).
pattern([c,c,w,c,c,w], 1).

pattern([c,w,c,c,w,c,c], 1).
pattern([c,w,c,c,w,c], 1).
pattern([c,w,c,c,w], 1).

pattern([w,c,c,w,c,c], 1).
pattern([w,c,c,w,c], 1).
pattern([w,c,c,w], 1).

% - w - - - w -
pattern([c,w,c,c,w,c], 1).
pattern([c,w,c,c,w], 1).

pattern([w,c,c,w,c], 1).
pattern([w,c,c,w], 1).

/**
 * Pure Pattern with 3 stones.
 * ! TODO
 * 40
 */

% - - w w w - -
pattern([c,c,w,w,w,c,c], 1). % SENTE
pattern([c,c,w,w,w,c], 1). % SENTE
pattern([c,c,w,w,w], 1).

pattern([c,w,w,w,c,c], 1). % SENTE
pattern([c,w,w,w,c], 1).
pattern([c,w,w,w], 1).

pattern([w,w,w,c,c], 1).
pattern([w,w,w,c], 1).
pattern([w,w,w], 1).

% - w w - w -   @   - w - w w -
pattern([c,w,w,c,w,c], 1). % SENTE
pattern([c,w,w,c,w], 1).

pattern([w,w,c,w,c], 1).
pattern([w,w,c,w], 1).

pattern([c,w,c,w,w,c], 1). % SENTE
pattern([c,w,c,w,w], 1).

pattern([w,c,w,w,c], 1).
pattern([w,c,w,w], 1).

% - - w - w - w - -
pattern([c,c,w,c,w,c,w,c,c], 1).
pattern([c,c,w,c,w,c,w,c], 1).
pattern([c,c,w,c,w,c,w], 1).

pattern([c,w,c,w,c,w,c,c], 1).
pattern([c,w,c,w,c,w,c], 1).
pattern([c,w,c,w,c,w], 1).

pattern([w,c,w,c,w,c,c], 1).
pattern([w,c,w,c,w,c], 1).
pattern([w,c,w,c,w], 1).

% - w - - w - w - -   @   - - w - w - - w -
pattern([c,w,c,c,w,c,w,c,c], 1).
pattern([c,w,c,c,w,c,w,c], 1).
pattern([c,w,c,c,w,c,w], 1).

pattern([w,c,c,w,c,w,c,c], 1).
pattern([w,c,c,w,c,w,c], 1).
pattern([w,c,c,w,c,w], 1).

pattern([c,c,w,c,w,c,c,w,c], 1).
pattern([c,c,w,c,w,c,c,w], 1).

pattern([c,w,c,w,c,c,w,c], 1).
pattern([c,w,c,w,c,c,w], 1).

pattern([w,c,w,c,c,w,c], 1).
pattern([w,c,w,c,c,w], 1).

/**
 * Pure Pattern with 4 stones.
 * ! TODO
 * 23
 */

% - w w w w -
pattern([c,w,w,w,w,c], 2 ** 60). % WIN IN 1.
pattern([c,w,w,w,w], 1). % SENTE

pattern([w,w,w,w,c], 1). % SENTE
pattern([w,w,w,w], 1).

% - - w w w - w   @   w - w w w - -
pattern([c,c,w,w,w,c,w], 1). % SENTE 2
pattern([c,w,w,w,c,w], 1). % SENTE
pattern([w,w,w,c,w], 1). % SENTE

pattern([w,c,w,w,w,c,c], 1). % SENTE 2
pattern([w,c,w,w,w,c], 1). % SENTE
pattern([w,c,w,w,w], 1). % SENTE

% - w - w w - w -
pattern([c,w,c,w,w,c,w,c], 1). % SENTE 2
pattern([c,w,c,w,w,c,w], 1). % SENTE

pattern([w,c,w,w,c,w,c], 1). % SENTE
pattern([w,c,w,w,c,w], 1).

% - - w - w - w - w - -
pattern([c,c,w,c,w,c,w,c,w,c,c], 1). % SENTE
pattern([c,c,w,c,w,c,w,c,w,c], 1). % SENTE
pattern([c,c,w,c,w,c,w,c,w], 1). % SENTE

pattern([c,w,c,w,c,w,c,w,c,c], 1). % SENTE
pattern([c,w,c,w,c,w,c,w,c], 1). % SENTE
pattern([c,w,c,w,c,w,c,w], 1). % SENTE

pattern([w,c,w,c,w,c,w,c,c], 1). % SENTE
pattern([w,c,w,c,w,c,w,c], 1). % SENTE
pattern([w,c,w,c,w,c,w], 1). % SENTE

/**
 * Pure Pattern with 5 stones.
 * ! TODO
 * 29
 */

% w w w w w
pattern([w,w,w,w,w], 2 ** 90). % WIN.

% w - w w w - w
pattern([w,c,w,w,w,c,w], 2 ** 60). % WIN IN 1.

% - w w - w - w w -
pattern([c,w,w,c,w,c,w,w,c], 1). % SENTE 2
pattern([c,w,w,c,w,c,w,w], 1). % SENTE

pattern([w,w,c,w,c,w,w,c], 1). % SENTE
pattern([w,w,c,w,c,w,w], 1).

% - - w w w - w w   @   w w - w w w - -
pattern([c,c,w,w,w,c,w,w], 1). % SENTE 2
pattern([c,w,w,w,c,w,w], 1). % SENTE
pattern([w,w,w,c,w,w], 1). % SENTE

pattern([w,w,c,w,w,w,c,c], 1). % SENTE 2
pattern([w,w,c,w,w,w,c], 1). % SENTE
pattern([w,w,c,w,w,w], 1). % SENTE

% - w - w w - w w   @   w w - w w - w -
pattern([c,w,c,w,w,c,w,w], 1). % SENTE 2
pattern([w,c,w,w,c,w,w], 1). % SENTE

pattern([w,w,c,w,w,c,w,c], 1). % SENTE 2
pattern([w,w,c,w,w,c,w], 1). % SENTE

% - - w - w - w - w - w - -
pattern([c,c,w,c,w,c,w,c,w,c,w,c,c], 1). % SENTE 3
pattern([c,c,w,c,w,c,w,c,w,c,w,c], 1). % SENTE 2
pattern([c,c,w,c,w,c,w,c,w,c,w], 1). % SENTE 2

pattern([c,w,c,w,c,w,c,w,c,w,c,c], 1). % SENTE 2
pattern([c,w,c,w,c,w,c,w,c,w,c], 1). % SENTE 2
pattern([c,w,c,w,c,w,c,w,c,w], 1). % SENTE 2

pattern([w,c,w,c,w,c,w,c,w,c,c], 1). % SENTE 2
pattern([w,c,w,c,w,c,w,c,w,c], 1). % SENTE 2
pattern([w,c,w,c,w,c,w,c,w], 1). % SENTE 2

% - w - w w - w - w   @   w - w - w w - w -
pattern([c,w,c,w,w,c,w,c,w], 1). % SENTE 2
pattern([w,c,w,w,c,w,c,w], 1). % SENTE

pattern([w,c,w,c,w,w,c,w,c], 1). % SENTE 2
pattern([w,c,w,c,w,w,c,w], 1). % SENTE

/**
 * Pure Pattern with 6 stones.
 * ! TODO
 * 14
 */

% - - w w w - w w w - -
pattern([c,c,w,w,w,c,w,w,w,c,c], 1). % SENTE 3
pattern([c,c,w,w,w,c,w,w,w,c], 1). % SENTE 2
pattern([c,c,w,w,w,c,w,w,w], 1). % SENTE 2

pattern([c,w,w,w,c,w,w,w,c,c], 1). % SENTE 2
pattern([c,w,w,w,c,w,w,w,c], 1). % SENTE
pattern([c,w,w,w,c,w,w,w], 1). % SENTE

pattern([w,w,w,c,w,w,w,c,c], 1). % SENTE 2
pattern([w,w,w,c,w,w,w,c], 1). % SENTE
pattern([w,w,w,c,w,w,w], 1). % SENTE

% w w - w w - w w
pattern([w,w,c,w,w,c,w,w], 2 ** 60). % WIN IN 1.

% - w - w w - w w w - -
pattern([c,w,c,w,w,c,w,w,w,c,c], 1). % SENTE 3
pattern([c,w,c,w,w,c,w,w,w,c], 1). % SENTE 2
pattern([c,w,c,w,w,c,w,w,w], 1). % SENTE 2

pattern([w,c,w,w,c,w,w,w,c,c], 1). % SENTE 2
pattern([w,c,w,w,c,w,w,w,c], 1). % SENTE
pattern([w,c,w,w,c,w,w,w], 1). % SENTE

pattern([c,c,w,w,w,c,w,w,c,w,c], 1). % SENTE 3
pattern([c,c,w,w,w,c,w,w,c,w], 1). % SENTE 2

pattern([c,w,w,w,c,w,w,c,w,c], 1). % SENTE 2
pattern([c,w,w,w,c,w,w,c,w], 1). % SENTE

pattern([w,w,w,c,w,w,c,w,c], 1). % SENTE 2
pattern([w,w,w,c,w,w,c,w], 1). % SENTE

/**
 * dynamic score/2
 * score(+Pattern, -Score).
 *   Uses pattern/2 to score a given pattern for white or black.
 */
 :- abolish(score/2). % reload
 :- findall([Pattern, Score], pattern(Pattern, Score), List),
    (   foreach([WPattern, WScore], List)
    do  (   list_reversal(WPattern, BPattern),
            BScore is -WScore,
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
captures_score([C,C], 0).

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
    captures_score(Bc, Wc, WScore),
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
