:- module(bomb2, [defuse/6, isNewState/2]).

use_module(library(lists)).

count(0, []).
count(Count, [Head|Tail]) :- count(TailCount, Tail), Count is TailCount + 1.

% (Volume,Current),(Volume,Current),Needed

writeSteps([]).
writeSteps([Head|Tail]) :-
  write(Head), nl,
  writeSteps(Tail).

isNewState(A,B) :-
  %write('Checking: '), write(A), write(' is in: '), write(B), nl,
  \+ memberchk(A, B).

% Status
isNotFull(A,B) :- A > B.

fillA(A,B) :- write('Fill A'), nl.
fillB(C,D) :- write('Fill B'), nl.

% Empty
emptyA() :- write('Empty A'), nl.
emptyB() :- write('Empty B'), nl.

bFromA(A,B,C,D,X,Y) :-
  LIQUID is B + D,
  % B can hold all liquid
  LIQUID =< C,
  X is 0,
  Y is LIQUID.

bFromA(A,B,C,D,X,Y) :-
  LIQUID is B + D,
  % B cannot hold all liquid
  LIQUID > C,
  X is LIQUID - C,
  Y is LIQUID - X.

aFromB(A,B,C,D,X,Y) :-
  LIQUID is B + D,
  % A can hold all liquid
  LIQUID =< A,
  Y is 0,
  X is LIQUID.

aFromB(A,B,C,D,X,Y) :-
  LIQUID is B + D,
  % A cannot hold all liquid
  LIQUID > A,
  Y is LIQUID - A,
  X is LIQUID - Y.

% Defused! Success!!
defuse(_,_, _,_, 0, _) :- write('Nothing to Defuse!'), nl.

defuse(_,B, _,D, E, H) :-
  B is E; D is E; E is B + D,
  count(STEPS, H),
  write('---- defuse 1 - Defused Successfully in '), write(STEPS), write(' Steps! ----'), nl,
  writeSteps(H).

% Fill A
defuse(A,B, C,D, E, H) :-
  A =\= B,
  isNotFull(C,D),
  isNewState((A,D), H),
  fillA(A,B),
  append([(A,D)], H, NEW_H),
  defuse(A,A,C,D,E,NEW_H).

% Fill B
defuse(A,B, C,D, E, H) :-
  C =\= D,
  isNotFull(A,B),
  isNewState((B,C), H),
  fillB(C,D),
  append([(B,C)], H, NEW_H),
  defuse(A,B,C,C,E,NEW_H).

% Fill B from A
defuse(A,B, C,D, E, H) :-
  %\+ B is E; \+ D is E; \+ E is B + D,
  bFromA(A,B,C,D,X,Y),
  isNewState((X,Y), H),
  write('Fill B from A'), nl,
  append([(X,Y)], H, NEW_H),
  defuse(A,X,C,Y,E,NEW_H).

% Fill A from B
defuse(A,B, C,D, E, H) :-
  aFromB(A,B,C,D,X,Y),
  isNewState((X,Y), H),
  write('Fill A from B'), nl,
  append([(X,Y)], H, NEW_H),
  defuse(A,X,C,Y,E,NEW_H).

% Empty A
defuse(A,B, C,D, E, H) :-
  B > 0,
  isNewState((0,D), H),
  emptyA(),
  append([(0,D)], H, NEW_H),
  defuse(A,0, C,D,E, NEW_H).

% Empty B
defuse(A,B, C,D, E, H) :-
  D > 0,
  isNewState((B,0), H),
  emptyB(),
  append([(B,0)], H, NEW_H),
  defuse(A,B, C,0,E, NEW_H).