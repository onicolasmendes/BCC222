module Aula02 where

xor :: Bool -> Bool -> Bool
xor True False = True
xor False True = True
xor  _ _ = False


existsPositve :: [Int] -> Bool
existsPositve [] = False
existsPositve (x:xs)
    |x > 0 = True
    |otherwise = existsPositve xs
