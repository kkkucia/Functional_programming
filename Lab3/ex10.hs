isSortedAsc :: Ord a => [a] -> Bool
isSortedAsc xs = and $ zipWith (<) xs (tail xs)
