import Data.Char
import System.IO

main :: IO ()
main = do
    contents <- readFile "girlfriend.txt"
    putStr contents
    writeFile "girlfriendcaps.txt" (map toUpper contents)
