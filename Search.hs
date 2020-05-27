linSearch :: Eq a => a -> [(a,b)] -> Maybe (a,b)
linSearch _ [] = Nothing
linSearch key ((a,b):xs) | key == a = Just (a,b)
linSearch key ((a,b):xs) | otherwise = linSearch key xs

binSearch :: (Eq a, Ord a) => a -> [(a,b)] -> Maybe b
-- list must be sorted
binSearch _ [] = Nothing
binSearch key xs | key == mid = Just $ snd (xs !! indexMid)
                 | key > mid = binSearch key (take indexMid xs)
                 | otherwise = binSearch key (drop (indexMid+1) xs) where
    indexMid = (length xs) `div` 2 + 1
    mid = fst (xs !! indexMid)
