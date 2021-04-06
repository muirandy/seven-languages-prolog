:- begin_tests(bomb).
:- use_module(bomb).

test(newState) :- isNewState((1,1),[]).
test(newState) :- isNewState((1,1),[(1,2)]).
test(newState) :- \+ isNewState((1,2),[(1,2)]).

test(notFull) :- \+ isNotFull(1,1).
test(notFull) :- isNotFull(2,1).

test(alreadySolved) :- alreadySolved(1,0,1).
test(alreadySolved) :- alreadySolved(0,1,1).
test(alreadySolved) :- alreadySolved(1,1,1).
test(alreadySolved) :- alreadySolved(1,1,2).
test(alreadySolved) :- \+ alreadySolved(1,1,3).
test(isNotSolvedAlready) :- \+ isNotSolvedAlready(1,0,1).
test(isNotSolvedAlready) :- \+ isNotSolvedAlready(0,1,1).
test(isNotSolvedAlready) :- \+ isNotSolvedAlready(1,1,1).
test(isNotSolvedAlready) :- \+ isNotSolvedAlready(1,1,2).
test(isNotSolvedAlready) :- isNotSolvedAlready(1,1,3).

% (Volume,Current),(Volume,Current),Needed,Turns,History

test(zero) :- defuse(1,0, 1,0, 0).
test(zero) :- defuse(1,1, 1,0, 0).
test(zero) :- defuse(1,0, 1,1, 0).
test(zero) :- defuse(1,1, 5,0, 1).
test(zero) :- defuse(5,0, 1,1, 1).
test(zero) :- defuse(1,1, 1,1, 2).

test(empty) :- defuse(1,1, 2,2, 2).

test(one) :- defuse(1,0, 5,0, 1).
test(one) :- defuse(5,0, 1,0, 1).

test(two) :- defuse(1,0, 1,1, 2).
test(two) :- defuse(1,1, 1,0, 2).

test(two) :- defuse(1,0, 1,0, 2).

test(six) :- defuse(3,0, 5,0, 4).

:- end_tests(bomb).
