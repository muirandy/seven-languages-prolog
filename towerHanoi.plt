:- begin_tests(towerHanoi).
:- use_module(towerHanoi).

test(one) :- move(1,source,target,aux).
test(two) :- move(2,source,target,aux).
test(three) :- move(3,source,target,aux).
test(four) :- move(4,source,target,aux).
test(five) :- move(5,source,target,aux).


:- end_tests(towerHanoi).