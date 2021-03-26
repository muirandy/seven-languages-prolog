:- module(palindrome, [palindrome/1]).

palindrome(Number) :- number_to_chars(Number, L),
    reverse(L, R), L = R.
