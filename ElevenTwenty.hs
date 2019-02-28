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
-- Answers to problems 11 to 20, questions found here: https://wiki.haskell.org/99_questions/


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


--Problem 14
duplicate :: [a] -> [a]
duplicate [] = []
duplicate [x] = [x] ++ [x]
duplicate (x:xs) = [x] ++ [x] ++ (duplicate xs)

