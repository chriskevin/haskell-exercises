module ListComprehension where

lcMap :: Num a => [a] -> [a]
lcMap xs = [x*2 | x <- xs]

lcMapExample :: [Integer]
lcMapExample = lcMap [1..10]

lcFilter :: Ord a => Num a => [a] -> [a]
lcFilter xs = [x | x <- xs, x > 3]

lcFilterExample :: [Integer]
lcFilterExample = lcFilter [1..10]

lcMultiple :: Num a => [a] -> [a] -> [a]
lcMultiple xs ys = [x+y | x <- xs, y <- ys]

removeNonUppercase :: String -> String
removeNonUppercase s = [c | c <- s, c `elem` ['A'..'Z']]