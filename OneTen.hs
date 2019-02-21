module OneTen 
(
  myLast,
  myButLast,
  elementAt,
  myReverse  
)
  where

-- FILE: OneTen.hs
-- AUTHOR: Nathan Robertson
-- Answers to problems 1 to 10, questions found here: https://wiki.haskell.org/99_questions/

-- Problem 1
myLast :: [a] -> a
myLast [x] = x
myLast li = myLast $ tail $ li

-- Problem 2
-- Find the last but one element of a list. 
myButLast :: [a] -> a
myButLast [x, last] = x
myButLast [x] = x
myButLast li = myButLast $ tail li

-- Problem 3
-- Find the K'th element of a list. The first element in the list is number 1. 
elementAt :: (Eq t, Num t) => [a] -> t -> a
elementAt li 1 = head li
elementAt li n = elementAt (tail li) (n - 1)

-- Problem 4
-- Find the number of elements of a list. 
myLength :: Num p => [a] -> p
myLength [] = 0
myLength li = (myLength (tail li)) + 1

-- Problem 5
-- Reverse a list
myReverse :: [a] -> [a]
myReverse [] = []
myReverse li = (myReverse (tail li)) ++ [head li]

-- Problem 6
-- Find out whether a list is a palindrome. A palindrome can be read forward or backward; e.g. (x a m a x). 
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome li = li == myReverse li

-- Problem 7
{-
Flatten a nested list structure.
Transform a list, possibly holding lists as elements into a `flat' list by replacing each list with its elements (recursively). 
We have to define a new data type, because lists in Haskell are homogeneous
-}
data NestedList a = Elem a | List [NestedList a] deriving (Show)

myFlatten (Elem x) = [x]
myFlatten (List x) = concatMap myFlatten x


-- Problem 8
{-
Eliminate consecutive duplicates of list elements.
If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed. 
-}
compress :: (Eq a) => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:xs)
    | x == (head xs) = x:(compress (tail xs))
    | otherwise = x:(compress xs)


-- Problem 9
-- Pack consecutive duplicates of list elements into sublists
pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack (x:xs) = [takeWhile (\item -> x == item) (x:xs)] ++ (pack (dropWhile (\item -> x == item) xs)) 
          

-- Problem 10
{-
Run-length encoding of a list. Use the result of problem P09 to implement the so-called run-length encoding data compression method. Consecutive duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the element E
-}
encode [] = []
encode xs = (zip (map (\subList -> length subList) (pack xs)) (map (\subList -> (head subList)) (pack xs)))