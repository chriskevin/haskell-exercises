module PhoneBook
( findKey
, inPhoneBook
, phoneBookToMap
) where

import Data.Char
import qualified Data.Map as Map

type AssocList k v = [(k, v)]

type PhoneNumber = String
type Name = String
type PhoneBook = AssocList Name PhoneNumber

phoneBook :: PhoneBook
phoneBook =
    [("betty", "555-2938")
    ,("bonnie", "452-2928")
    ,("patsy", "493-2928")
    ,("lucille", "205-2928")
    ,("wendy", "939-8282")
    ,("penny", "853-2492")
    ,("wendy", "939-8455")
    ]

findKey :: (Eq k) => k -> AssocList k v -> Maybe v
findKey key xs = foldr (\(k,v) acc -> if key == k then Just v else acc) Nothing xs

inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name pnumber pbook = (name, pnumber) `elem` pbook

string2Digits :: String -> [Int]
string2Digits = map digitToInt . filter isDigit

phoneBookToMap :: (Ord k) => AssocList k a -> Map.Map k [a]
phoneBookToMap xs = Map.fromListWith (++) $ map (\(k,v) -> (k, [v])) xs
