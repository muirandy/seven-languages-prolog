:- begin_tests(palindrome).
:- use_module(palindrome).

test(negative, fail) :- palindrome(-1).
test(zero) :- palindrome(0).
test(nine) :- palindrome(9).
test(ten, fail) :- palindrome(10).
test(eleven) :- palindrome(11).


:- end_tests(palindrome).

