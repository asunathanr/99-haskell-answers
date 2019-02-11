module OneTen (myLast, myButLast, myReverse) where

-- FILE: OneTen.hs
-- AUTHOR: Nathan Robertson
-- Answers to problems 1 to 10, questions found here: https://wiki.haskell.org/99_questions/

-- Problem 1
myLast :: [a] -> a
myLast [x] = x
myLast li = myLast $ tail $ li

-- Problem 2
myButLast :: [a] -> a
myButLast [x, last] = x
myButLast [x] = x
myButLast li = myButLast $ tail li

-- Problem 3
elementAt :: (Eq t, Num t) => [a] -> t -> a
elementAt li 1 = head li
elementAt li n = elementAt (tail li) (n - 1)

-- Problem 4
myLength :: Num p => [a] -> p
myLength [] = 0
myLength li = (myLength (tail li)) + 1

-- Problem 5
myReverse [] = []
myReverse li = (myReverse (tail li)) ++ [head li]

-- Problem 6
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome li = li == myReverse li