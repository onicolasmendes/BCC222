module Aula04 where

primes :: Int -> [Int]
primes n = [x | x <- [2..n], isPrime x]
    where
        isPrime x
            |(length [p | p <- [1 .. x], mod x p == 0]) > 2 = False
            |otherwise = True

minList :: Num a => Ord a => [a] ->Maybe a 
minList [] = Nothing
minList (x : []) = Just x
minList (x: y : xs)
    |x < y = minList (x:xs)
    | otherwise = minList (y : xs)

andList :: [Bool] -> Bool
andList [] = True
andList (x : xs) = x && andList xs

orList :: [Bool] -> Bool
orList [] = False
orList (x : xs) = x || orList xs