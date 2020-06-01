-- Recursion

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum' of empty list"
maximum' [x] = x
maximum' (x:xs) = max x (maximum xs)

replicate' :: Int -> a -> [a]
replicate' n x
    | n <= 0 = []
    | otherwise = x : replicate' (n - 1) x

take' :: Int -> [a] -> [a]
take' n _
    | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x : take' (n - 1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' [x] = [x]
reverse' (x:xs) = reverse' xs ++ [x]

repeat' :: a -> [a]
repeat' x = x : repeat x

zip' :: [a] -> [b] -> [(a, b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs)
    | a == x = True
    | otherwise = a `elem'` xs

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort [x] = [x]
quicksort (x:xs) =
    let smallerOrEqual = [a | a <- xs, a <= x]
        larger = [a | a <- xs, a > x]
    in quicksort smallerOrEqual ++ [x] ++ quicksort larger

-- https://smthngsmwhr.wordpress.com/2012/11/09/sorting-algorithms-in-haskell/#bubble_sort
bubblesort'iter :: (Ord a) => [a] -> [a]
bubblesort'iter (x:y:xs)
    | x > y = y : bubblesort'iter (x:xs)
    | otherwise = x : bubblesort'iter (y:xs)
bubblesort'iter (x) = (x)

bubblesort' :: (Ord a) => [a] -> Int -> [a]
bubblesort' xs i
    | i == (length xs) = xs
    | otherwise = bubblesort' (bubblesort'iter xs) (i + 1)

bubblesort :: (Ord a) => [a] -> [a]
bubblesort xs = bubblesort' xs 0
