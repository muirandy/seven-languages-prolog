:- begin_tests(sudoku).
:- use_module(sudoku).

test(noBlanks) :- sudoku([4,1,2,3,
			  2,3,4,1,
			  1,2,3,4,
                          3,4,1,2], Solution),
    Solution = [4,1,2,3,
                2,3,4,1,
                1,2,3,4,
		3,4,1,2].

test(range, fail) :- sudoku([4,1,2,9,
                      2,3,4,5,
                      1,2,3,4,
                      3,4,5,6], _).

test(tooSmall, fail) :- sudoku([4,1,2,3,
                      2,3,4,1,
                      1,2,3,4,
                      3,4,1], _).

test(tooBig, fail) :- sudoku([4,1,2,3,
                      2,3,4,1,
                      1,2,3,4,
                      3,4,1,2,
                      2], _).

:- end_tests(sudoku).
