import ElevenTwenty

-- Problem 21
insertAt :: a -> [a] -> Int -> [a]
insertAt x xs n = (take (n - 1) xs) ++ [x] ++ (drop (n - 1) xs)


-- Problem 22
-- Create a list containing all integers within a given range. 
-- Note: Both begin and end are inclusive
range begin end
  | begin < end = [begin] ++ (range (begin + 1) end)
  | begin > end = [begin] ++ (range (begin - 1) end)
  | otherwise = [begin] 