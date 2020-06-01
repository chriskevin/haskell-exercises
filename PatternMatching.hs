lucky :: Int -> String
lucky 7 = "LUCKY NUMBER SEVEN"
lucky x = "Sorry, you're out of luck, pal!"


sayMe :: Int -> String
sayMe 1 = "ONE!"
sayMe 2 = "TWO!"
sayMe 3 = "THREE!"
sayMe x = "Not between 1 and 3"

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

charName :: Char -> String
charName 'a' = "Albert"
charName 'b' = "Broseph"
charName 'c' = "Cecil"
charName x = "Sorry, don't know you!"


-- Pattern matching tuples
addVectors :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- Pattern matching lists
tell :: (Show a) => [a] -> String
tell [] = "This list is empty"
tell (x:[]) = "This list has one element: " ++ show x
tell (x:y:[]) = "This list has two elements: " ++ show x ++ " and " ++ show y
tell (x:y:_) = "This list is long. The first two elements are " ++ show x ++ " and " ++ show y

-- As-patterns
firstLetter :: String -> String
firstLetter "" = "Empty string, whoops!"
firstLetter all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

-- Guards + where binding
bmiTell :: Double -> Double -> String
bmiTell weight height
    | bmi <= skinny = "You're underweight, eat more!"
    | bmi <= normal = "Looking good!"
    | bmi <= fat = "You're overweight. Let's work out together!"
    | otherwise   = "You're obese. Go see a doctor."
    where bmi = (weight / height) ^ 2
          (skinny, normal, fat) = (18.5, 25.0, 30.0)

niceGreeting :: String
niceGreeting = "Hello! So very nice to see you,"

badGreeting :: String
badGreeting = "Oh! Pfft. It's you."

greet :: String -> String
greet "Juan" = niceGreeting ++ " Juan!"
greet "Fernando" = niceGreeting ++ " Fernando!"
greet name = badGreeting ++ " " ++ name

initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
    where (f:_) = firstname
          (l:_) = lastname

-- let expressions
cylinder :: Double -> Double -> Double
cylinder r h =
    let sideArea = 2 * pi * r * h
        topArea = pi * r ^ 2
    in sideArea + 2 * topArea

-- case expressions
head' :: [a] -> a
head' xs = case xs of [] -> error "No head for empty lists!"
                      (x:_) -> x

describeList :: [a] -> String
describeList ls = "The list is " ++ case ls of [] -> "empty."
                                               [x] -> "a singleton list."
                                               xs -> "a longer list."
