-- Various stats related programming projects

-- Find mean of given samples
-- Note: An empty foldable will throw a divide by zero exception
mean :: (Foldable t) => t Int -> Int
mean samples = (sum samples) `div` (length samples) 