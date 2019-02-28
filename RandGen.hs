module RandGen
(
  randMidSquare
)
  where

-- John von neumann's middle square algorithm
randMidSquare :: Integer -> Integer
randMidSquare prev = read (getMidSquare (show (prev ^ 2))) :: Integer

-- Helper for randMidSquare
getMidSquare :: [Char] -> [Char]
getMidSquare str = let origLength = (length str) `div` 2
                   in (take origLength (drop (origLength - 1) str))