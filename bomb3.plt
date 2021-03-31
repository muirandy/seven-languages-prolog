:- begin_tests(bomb3Safe).
:- use_module(bomb3Safe).

test(bFromA) :- bFromA(10,1,10,0,0,1).
test(bFromA) :- bFromA(5,4,5,4,3,5).

test(aFromB) :- aFromB(10,0,10,1,1,0).
test(aFromB) :- aFromB(5,4,5,4,5,3).

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

:- end_tests(bomb3Safe).
