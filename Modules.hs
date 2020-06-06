-- Import all
-- import Data.List

-- Import explicitly
-- import Data.List (filter, nub)

-- Import all, explicitly excluding
-- import Data.List hiding (nub)

-- Import qualified (with namespace)
-- import qualified Data.List

-- Import qualified with local alias
-- import qualified Data.List as List

import Data.Char
import Data.List

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

wordNums :: String -> [(String, Int)]
wordNums = map (\ws -> (head ws, length ws)) . group . sort . words

isIn :: (Eq a) => [a] -> [a] -> Bool
needle `isIn` haystack = any (needle `isPrefixOf`) $ tails haystack

digitSum :: Int -> Int
digitSum = sum . map digitToInt . show

firstTo :: Int -> Maybe Int
firstTo n = find (\x -> digitSum x == n) [1..]

firstTo40 :: Maybe Int
firstTo40 = firstTo 40
