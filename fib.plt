:- begin_tests(fib).
:- use_module(fib).

test(five) :- fib(5,5).
test(six) :- fib(6,8).

:- end_tests(fib).
