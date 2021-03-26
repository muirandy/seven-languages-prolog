:- module(bomb, [defuse/4]).

%defuse(_, _, 0, 0).
%defuse((A,B), _, B, 0).
defuse((A,B), _, A, 1).
%defuse(_, (A,B), B, 0).
defuse(_, (A,B), A, 1).

defuse((A,B), (C,D), E, 0) :- E is B + D.
defuse((A,B), (C,D), E, 0) :- E is B + D.



