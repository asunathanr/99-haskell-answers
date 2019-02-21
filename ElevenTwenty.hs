import OneTen

-- FILE: OneTen.hs
-- AUTHOR: Nathan Robertson
-- Answers to problems 11 to 20, questions found here: https://wiki.haskell.org/99_questions/


-- Problem 11
{-
  Modified run-length encoding.

Modify the result of problem 10 in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N E) lists. 
-}
data Encoding a = Single a | Multiple Int a
