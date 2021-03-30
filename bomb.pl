:- module(bomb, [defuse/4]).

% (Volume,Current),(Volume,Current),Needed,Turns

%defuse(_, _, 0, 0).
%defuse((A,B), _, B, 0).
%defuse(_, (A,B), B, 0).

% Defused! Success!!
defuse((A,B), (C,D), E, 0) :- E is B, write('Use Container 1'), nl.
defuse((A,B), (C,D), E, 0) :- E is D, write('Use Container 2'), nl.
defuse((A,B), (C,D), E, 0) :- E is B + D, write('Use both'), nl.

% Fill Container 1
%defuse((A,B), _, A, 1).
%defuse(_, (A,B), A, 1).
defuse((A,B), (C,D), E, X) :-
  A=E,
  defuse((A,A), (C,D), E, Y),
  X is Y+1,
  A>B,
  write('fill A'), nl.

% Fill Container 2
defuse((A,B), (C,D), E, X) :-
  C=E,
  defuse((A,B), (C,C), E, Y),
  X is Y+1,
  C>D,
  write('fill B'), nl.


%defuse((A,B), (C,D), E, 1) :- E is A + D.
%defuse((A,B), (C,D), E, 1) :- E is B + C.

%defuse((A,B), (C,D), E, 2) :- E is A + C.



