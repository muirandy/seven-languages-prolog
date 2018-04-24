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

:- end_tests(sudoku).
