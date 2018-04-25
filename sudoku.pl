:- module(sudoku, [sudoku/2]).

sudoku(Puzzle, Solution) :-
    Solution = Puzzle,
    max_list(Puzzle, M), M < 5,
     Puzzle = [S11,S12,S13,S14,
               S21,S22,S23,S24,
               S31,S32,S33,S34,
               S41,S42,S43,S44],
     Row1 = [S11,S12,S13,S14],
     Row2 = [S21,S22,S23,S24],
     Row3 = [S31,S32,S33,S34],
     Row4 = [S41,S42,S43,S44],
     is_set(Row1),
     is_set(Row2),
     is_set(Row3),
     is_set(Row4).


