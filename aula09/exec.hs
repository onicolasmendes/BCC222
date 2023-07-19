module Aula09 where

import Data.Char
import Test.QuickCheck

-- Exercicios dos Slides
data Person = Person {
        name :: String
        , age :: Int       
}

renders :: Person -> String
renders (Person name i) = name

mkName :: String -> Person
mkName [] = (Person [] 0)
mkName (x:xs) = (Person (x':xs') 0)
    where
        x' = toUpper x
        xs' = map toLower xs

comecaMaiuscula :: String -> Bool
comecaMaiuscula [] = True
comecaMaiuscula (x : xs) = isUpper x

validateString :: String -> Bool
validateString [] = True
validateString xs
    |(all isLetter xs) = comecaMaiuscula(renders(mkName xs))
    |otherwise = True
    
--validateString1 :: String -> Bool
--validateString1 [] = True
--validateString1 xs = (all isLetter xs) `implies` b
--    where
--        b = comecaMaiuscula xs'
--        xs' = renders (mkName xs)
    
-- Exercicio 1
inRange ::  Int -> Int -> [Int] -> [Int]
inRange _ _ [] = []
inRange x y (n : ns)
    |n>=x && n <= y = n : inRange x y ns
    |otherwise = inRange x y ns

inRangePropriedade :: Int -> Int -> [Int] -> Bool
inRangePropriedade _ _ [] = True
inRangePropriedade x y (n : ns)
    |n >= x && n <= y = inRangePropriedade x y ns
    |otherwise = False

testInRange :: Int -> Int -> [Int] -> Bool
testInRange _ _ [] = True
testInRange x y ns
    |y < x = True
    |otherwise = inRangePropriedade x y (inRange x y ns)