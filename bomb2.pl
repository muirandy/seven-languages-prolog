:- module(bomb2, [defuse/5]).

% (Volume,Current),(Volume,Current),Needed

% Defused! Success!!
defuse(_,B, _,_, E) :- E =:= B, write('Use Container 1'), nl.
defuse(_,_, _,D, E) :- E =:= D, write('Use Container 2'), nl.
defuse(_,B, _,D, E) :- E =:= B + D, write('Use both'), nl.

% Fill Container 1
defuse(A,B, C,D, E) :-
  sleep(2),
  E =\= B,
  E =\= D,
  write('Fill Container 1'), nl,
  defuse(A,A, C,D, E).

% Fill Container 2
defuse(A,B, C,D, E) :-
  sleep(2),
  E =\= B,
  E =\= D,
  write('Fill Container 2'), nl,
  defuse(A,B, C,C, E).
