module OneTen (myLast, myButLast) where

-- Answers to problems 1 to 10, questions found here: https://wiki.haskell.org/99_questions/

-- Problem 1
myLast [x] = x
myLast li = myLast $ tail $ li

-- Problem 2
myButLast [x, last] = x
myButLast li = myButLast $ tail li

-- Problem 3
elementAt li 1 = head li
elementAt li n = elementAt (tail li) (n - 1)
