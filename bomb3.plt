:- begin_tests(bomb2).
:- use_module(bomb2).

% (Volume,Current),(Volume,Current),Needed,Turns

test(zero) :- defuse(1,0, 1,0, 0).
test(zero) :- defuse(1,1, 1,0, 0).
test(zero) :- defuse(1,0, 1,1, 0).
test(zero) :- defuse(1,1, 5,0, 1).
test(zero) :- defuse(5,0, 1,1, 1).
test(zero) :- defuse(1,1, 1,1, 2).

test(one) :- defuse(1,0, 5,0, 1).
test(one) :- defuse(5,0, 1,0, 1).

%test(two) :- defuse(1,0, 1,1, 2).
%test(two) :- defuse(1,1, 1,0, 2).

%test(two) :- defuse(1,0, 1,0, 2).

:- end_tests(bomb2).
