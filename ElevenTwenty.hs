module ElevenTwenty
(
  encodeModified,
  decodeModified,
  duplicate
)
  where


import OneTen

-- FILE: OneTen.hs
-- AUTHOR: Nathan Robertson
-- Answers to problems 11 to 20, questions found here: https://wiki.haskell.org/99_questions


-- Problem 11
{-
  Modified run-length encoding.

  Modify the result of problem 10 in such a way that if an element has no duplicates it is simply copied
  into the result list. Only elements with duplicates are transferred as (N E) lists. 
-}
data Encoding a = Single a | Multiple (Int, a) deriving Show

encodeModified :: (Eq a) => [a] -> [Encoding a]
encodeModified li = map extractUnique (encode li)

-- Helper for problem 11
-- Is not exported as part of module.
extractUnique :: (Int, a) -> Encoding a
extractUnique (val, x)
    | val == 1 = Single x
    | otherwise = Multiple (val, x)


-- Problem 12
{-
  Decode a run-length encoded list.

  Given a run-length code list generated as specified in problem 11. Construct its uncompressed version. 
-}
decodeModified :: [Encoding a] -> [a]
decodeModified [] = []
decodeModified ((Single x):xs) = [x] ++ (decodeModified xs)
decodeModified ((Multiple (val, x)):xs) = (replicate val x) ++ (decodeModified xs)



-- Problem 13
--Run-length encoding of a list (direct solution).
--Implement the so-called run-length encoding data compression method directly. I.e. don't explicitly create the sublists containing the duplicates, as in problem 9, but only count them. As in problem P11, simplify the result list by replacing the singleton lists (1 X) by X. 
encodeDirect [] = []
encodeDirect li = [encodeOnce li] ++ (encodeDirect (dropWhile (\x -> x == (head li)) li))

-- Helper for problem 13
encodeOnce li = convertDuplicates (takeWhile (\x -> x == (head li)) li)

-- Helper for problem 13
convertDuplicates li =
  if length li == 1 then
    Single (head li)
  else
    Multiple ((length li), (head li))


--Problem 14
duplicate :: [a] -> [a]
duplicate [] = []
duplicate [x] = [x] ++ [x]
duplicate (x:xs) = [x] ++ [x] ++ (duplicate xs)


-- Problem 15
-- Replicate the elements of a list a given number of times.
repli [] n = []
repli xs n = (repeatHelper (head xs) n) ++ (repli (tail xs) n)

-- Helper for problem 15
-- Forms a list of value x repeated n times
-- Example: repeat 'a' 2 -> ['a', 'a']
repeatHelper x n =
   if n == 0 then
     []
  else
    [x] ++ (repeatHelper x (n - 1))


-- Problem 16
-- Drop every N'th element from a list
dropEvery :: [a] -> Int -> [a]
dropEvery [] n = []
dropEvery xs 0 = xs
dropEvery xs n = (take (n - 1) xs) ++ (dropEvery (drop n xs) n) 


-- Problem 17
{-
Split a list into two parts; the length of the first part is given.

Do not use any predefined predicates. 
Note: Previous version had two other patterns which covered base cases an
empty list and where n == 0 respectively.
The solution set just uses the one pattern below.
-}
split xs n = ((take n xs), (drop n xs))


-- Problem 18
{-
Extract a slice from a list.

Given two indices, i and k, the slice is the list containing the elements between the i'th and k'th element of the original list (both limits included). Start counting the elements with 1. 
-}
slice xs i k = (take (k - i + 1) (drop (i - 1) xs))
