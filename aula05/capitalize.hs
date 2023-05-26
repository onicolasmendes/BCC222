import Data.Char
capitalize :: String -> String
capitalize [] = []
capitalize xs = map toUpper xs