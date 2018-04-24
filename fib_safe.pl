fib(1, 1).
fib(2, 1).
fib(X, Y) :- myFib(X, Y, [1,1]).

count(0, []).
count(Count, [Head|Tail]) :- count(TailCount, Tail), Count is TailCount + 1.

addToSequence([Head1|[Head2|Tail]], [R, Head1]) :- R is Head1 + Head2. 

myFib(P, R, V) :- count(C, V), C = P, head(R, V).
myFib(P, R, V) :- addToSequence(V, V2), count(C, V2), \+(C = P), P2 is P - 1, myFib(P2, R, V2).
