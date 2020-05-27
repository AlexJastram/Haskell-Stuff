
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:[]) = [x]
quicksort (p:xs) = (quicksort left)++[p]++(quicksort right)
  where left = [x | x <- xs, x < p]
        right = [x | x <- xs, x >= p]


mergesort :: (Ord a) => [a] -> [a]
mergesort [] = []
mergesort [x] = [x]
mergesort xs = merge (mergesort left) (mergesort right) where
  split = (length xs) `div` 2
  left  = take split xs
  right = drop split xs
  merge [] ys = ys
  merge xs [] = xs
  merge (x:xs) (y:ys) | x<=y      = x : (merge xs (y:ys))
  merge (x:xs) (y:ys) | otherwise = y : (merge ys (x:xs))


bubblesort :: (Ord a) => [a] -> [a]
bubblesort [] = []
bubblesort (x:[]) = [x]
bubblesort xs =  bubblesort (init xs') ++ [last xs']
  where xs' = bubbleRise xs
        bubbleRise [] = []
        bubbleRise (a:[]) = [a]
        bubbleRise (a:b:xs) = (min a b) : bubbleRise ( (max a b):xs)

insertionsort :: (Ord a) => [a] -> [a]
insertionsort [] = []
insertionsort [x] = [x]
insertionsort (x:xs) = insert x (insertionsort xs) where
  insert y [] = [y]
  insert y (x:xs) | y <= x = y : (x:xs)
                  | otherwise = x : (insert y xs)
