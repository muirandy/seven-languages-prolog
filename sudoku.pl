:- module(sudoku, [sudoku/2]).

sudoku(Puzzle, Solution) :-
    Solution = Puzzle,
    max_list(Puzzle, M), M < 5.
    %fd_dom(Puzzle, 1, 4).

