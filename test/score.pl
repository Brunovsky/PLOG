% evaluate(List, Value)
test_evaluate :-
    random_list(10, [c,c,w], W),

    evaluate(W, V1),
    evaluate([c|W], V2), V2 >= V1,
    last(W, c, W1),
    evaluate(W1, V3), V3 >= V1,

    list_reversal(W, B),
    list_reversal(W1, B1),

    evaluate(B, V4), V4 is -V1,
    evaluate([c|B], V5), V5 is -V2,
    evaluate(B1, V6), V6 is -V3.

%captures_score([Wc,Bc], Score)
test_captures_score :-
    captures_score([3, 3], 0),
    captures_score([8, 8], 0),
    captures_score([6, 4], V1),
    captures_score([4, 6], V2), V2 is -V1,
    captures_score([0, 8], V3),
    captures_score([8, 0], V4), V4 is -V3.
    
    


test_score :- test_all(score, [
    test_evaluate,
    test_captures_score
]).