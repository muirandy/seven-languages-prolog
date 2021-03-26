:- begin_tests(bomb).
:- use_module(bomb).

test(zero) :- defuse((1,0), (1,0), 0, 0).
test(one) :- defuse((1,1), (5,0), 1, 0).
test(one) :- defuse((5,0), (1,1), 1, 0).
test(one) :- defuse((1,0), (5,0), 1, 1).
test(one) :- defuse((5,0), (1,0), 1, 1).

test(two) :- defuse((1,1), (1,1), 2, 0).
test(two) :- defuse((1,0), (1,1), 2, 1).
test(two) :- defuse((1,1), (1,0), 2, 1).

test(two) :- defuse((1,0), (1,0), 2, 2).

:- end_tests(bomb).
