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

test(gridTooSmall, fail) :- sudoku([4,1,2,3,
                      2,3,4,1,
                      1,2,3,4,
                      3,4,1], _).

test(gridTooBig, fail) :- sudoku([4,1,2,3,
                      2,3,4,1,
                      1,2,3,4,
                      3,4,1,2,
                      2], _).

test(noRepeatsOnRow1, fail) :- sudoku([4,1,2,4,
                      2,3,4,1,
                      1,2,3,4,
                      3,4,1,2],_).

test(noRepeatsOnRow2, fail) :- sudoku([4,1,2,3,
                      2,3,4,2,
                      1,2,3,4,
                      3,4,1,2],_).

test(noRepeatsOnRow3, fail) :- sudoku([4,1,2,3,
                      2,3,4,1,
                      1,2,3,1,
                      3,4,1,2],_).

test(noRepeatsOnRow4, fail) :- sudoku([4,1,2,3,
                      2,3,4,1,
                      1,2,3,4,
                      3,4,1,3],_).

:- end_tests(sudoku).
