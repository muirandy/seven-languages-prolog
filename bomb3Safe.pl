:- module(bomb3Safe, [
  defuse/5,
  defuse/6,
  isNotFull/2,
  alreadySolved/3,
  isNotSolvedAlready/3,
  isNewState/2,
  fillBFromA/6,
  fillAFromB/6,
  emptyA/6,
  emptyB/6,
  bFromA/6]).

use_module(library(lists)).

count(0, []).
count(Count, [Head|Tail]) :- count(TailCount, Tail), Count is TailCount + 1.

% (Volume,Current),(Volume,Current),Needed,History

writeSteps([]).
writeSteps([Head|Tail]) :-
  write(Head), nl,
  writeSteps(Tail).

writeAll(A,B,C,D,E,H) :-
  write('('), write(A), write(','), write(B), write(') '),
  write('('), write(C), write(','), write(D), write(') '),
  write('E='), write(E),
  write(' H: '), write(H), nl.

alreadySolved(B,D,E) :-
  B is E; D is E; E is B + D.

isNotSolvedAlready(B,D,E) :-
  \+ alreadySolved(B,D,E).

willBeSolved(B,D,E) :-
  write('---Next step?: '), write(B), write(','), write(D), nl.

%isExistingState((0,0),_).
isExistingState(A,B) :-
  memberchk(A, B).
isNewState(A,B) :-
  \+ isExistingState(A,B).


% Status
isNotFull(A,B) :- A > B.

fillA(A,B,C,D,E,H) :-
  isNotSolvedAlready(B,D,E),
  isNotFull(A,B),
  isNewState((A,D), H),
  write('Fill A'), nl,
  willBeSolved(A,D,E),
  append([(A,D)], H, NEW_H),
  defuse(A,A,C,D,E,NEW_H).

fillB(A,B,C,D,E,H) :-
  isNotSolvedAlready(B,D,E),
  isNotFull(C,D),
  isNewState((B,C), H),
  write('Fill B'), nl,
  append([(B,C)], H, NEW_H),
  defuse(A,B,C,C,E,NEW_H).

% Empty
emptyA(A,B,C,D,E,H) :-
  isNotSolvedAlready(B,D,E),
  B > 0,
  D > 0,
  C =\= D,
  isNewState((0,D), H),
  append([(0,D)], H, NEW_H),
  write('Empty A'), nl,
  writeAll(A,0,C,D,E,NEW_H),
  defuse(A,0, C,D,E, NEW_H).

emptyB(A,B,C,D,E,H) :-
   isNotSolvedAlready(B,D,E),
   B > 0,
   D > 0,
   A =\= B,
   isNewState((B,0), H),
   write('Empty B'), nl,
   append([(B,0)], H, NEW_H),
   defuse(A,B, C,0, E, NEW_H).

bFromA(_,B,C,D,X,Y) :-
  LIQUID is B + D,
  % B can hold all liquid
  LIQUID =< C,
  X is 0,
  Y is LIQUID.

bFromA(_,B,C,D,X,Y) :-
  LIQUID is B + D,
  % B cannot hold all liquid
  LIQUID > C,
  X is LIQUID - C,
  Y is C.

fillBFromA(A,B, C,D, E, H) :-
  B > 0,
  isNotFull(C,D),
  isNotSolvedAlready(B,D,E),
  bFromA(A,B,C,D,X,Y),
  isNewState((X,Y), H),
  append([(X,Y)], H, NEW_H),
  defuse(A,X,C,Y,E,NEW_H).

fillAFromB(A,B,C,D,E,H) :-
  D > 0,
  isNotFull(A,B),
  isNotSolvedAlready(B,D,E),
  bFromA(C,D,A,B,Y,X),
  isNewState((X,Y), H),
  append([(X,Y)], H, NEW_H),
  defuse(A,X,C,Y,E,NEW_H).

writeDefuseSuccessfully(H) :-
  count(STEPS, H),
  write('---- Defused Successfully in '), write(STEPS), write(' Steps! ----'), nl,
  writeSteps(H).

% Public method
defuse(A,B,C,D,E) :- defuse(A,B,C,D,E,[]).

defuse(_,_, _,_, 0, _) :- write('Nothing to Defuse!'), nl.

defuse(_,B, _,_, E, H) :-
  B is E,
  writeDefuseSuccessfully(H).

defuse(_,_, _,D, E, H) :-
  D is E,
  writeDefuseSuccessfully(H).

defuse(_,B, _,D, E, H) :-
  E is B + D,
  writeDefuseSuccessfully(H).

defuse(A,B, C,D, E, H) :- fillA(A,B,C,D,E,H).

defuse(A,B, C,D, E, H) :- fillB(A,B,C,D,E,H).

defuse(A,B, C,D, E, H) :- fillBFromA(A,B,C,D,E,H).

defuse(A,B, C,D, E, H) :- fillAFromB(A,B,C,D,E,H).

defuse(A,B, C,D, E, H) :- emptyA(A,B,C,D,E,H).

defuse(A,B, C,D, E, H) :- emptyB(A,B,C,D,E,H).