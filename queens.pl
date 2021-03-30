:- module(queens, [queens/1, differentRows/2, differentCols/2]).

count(0, []).
count(Count, [Head|Tail]) :- count(TailCount, Tail), Count is TailCount + 1.

validSquare((A,B)) :- A > 0, A < 9, B >0, B < 9.

validSquares([]).
validSquares([Head|Tail]) :- validSquare(Head), validSquares(Tail).

differentRows([], []).
differentRows([(X,Y)|Tail], [X|Tail2]) :- differentRows(Tail, Tail2).

differentCols([], []).
differentCols([(X,Y)|Tail], [Y|Tail2]) :- differentCols(Tail, Tail2).



%valid([]).
%valid([Head|Tail]) :- all_different(Head), valid(Tail).

queens(Q) :-
  count(8, Q),
  validSquares(Q),
  differentRows(Q),
  differentCols(Q).
