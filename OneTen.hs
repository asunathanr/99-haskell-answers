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
myReverse :: [a] -> [a]
myReverse [] = []
myReverse li = (myReverse (tail li)) ++ [head li]

-- Problem 6
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome li = li == myReverse li

-- Problem 7
data NestedList a = Elem a | List [NestedList a]
{-
myFlatten (Elem x) = x
myFlatten (List [Elem x]) =  x 
myFlatten (List [List xs]) = myFlatten xs 
-}

-- Problem 8
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
pack (x:xs) = [(takeWhile (\item -> x == item) (x:xs))] ++ (pack (dropWhile (\item -> x == item) xs)) 
          

-- Problem 10