:- begin_tests(concat).
:- use_module(concat).

test(empty) :- concatenate([],[],[]).
test(single) :- concatenate([1],[],[1]).
test(single) :- concatenate([],[1],[1]).
test(both) :- concatenate([1],[2],[1,2]).
test(both) :- concatenate([1,2],[3,4],[1,2,3,4]).


:- end_tests(concat).
