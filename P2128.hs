import ElevenTwenty

-- Problem 21
insertAt x xs n = (take (n - 1) xs) ++ [x] ++ (drop (n - 1) xs)