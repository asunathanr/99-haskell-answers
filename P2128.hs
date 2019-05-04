import System.Random
import ElevenTwenty

-- Problem 21
insertAt :: a -> [a] -> Int -> [a]
insertAt x xs n = (take (n - 1) xs) ++ [x] ++ (drop (n - 1) xs)


-- Problem 22
-- Create a list containing all integers within a given range. 
-- Note: Both begin and end are inclusive
range :: (Ord a, Num a) => a -> a -> [a]
range begin end
  | begin < end = [begin] ++ (range (begin + 1) end)
  | begin > end = [begin] ++ (range (begin - 1) end)
  | otherwise = [begin]


-- Problem 23
-- Extract a given number of randomly selected elements from a list. 
rnd_select :: [a] -> Int -> IO [a]
rnd_select [] n = do
  return []
rnd_select xs 0 = do
  return []
rnd_select xs n = 
  do 
    index <- (getStdRandom (randomR (1, (length xs))))
    nextLi <- (rnd_select ((take (index - 1) xs) ++ (drop index xs)) (n - 1))
    return ((head (take index xs)):nextLi)


{-
Problem 24
Lotto: Draw N different random numbers from the set 1..M. 
Note: Solution is inspired by solutions to 23
https://wiki.haskell.org/99_questions/Solutions/23
-}
diff_select :: Int -> Int -> IO [Int]
diff_select n m = getStdRandom $ make_lotto [1..m] n

-- Helper for problem 24
make_lotto _ 0 gen = ([], gen)
make_lotto [] _ gen = ([], gen)
make_lotto xs n gen
    | n == (length xs) = (xs, gen)
    | otherwise = make_lotto (removeAt (k + 1) xs) n gen'
                  where (k, gen') = randomR (0, (length xs) - 1) gen