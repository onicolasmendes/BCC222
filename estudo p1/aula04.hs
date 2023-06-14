module Aula04 where
import Prelude hiding (concatMap)
import Data.Char

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

indexOf :: Int -> [Int] -> Int
indexOf _ [] = -1
indexOf x xs = index x xs 0
    where
        index x (y:ys) ac
            |x == y = ac
            |otherwise = index x ys (ac+1)
        index x [] ac = -1

allNats :: [Int] -> Bool
allNats = foldr step True
    where
        step x ac
            |x > 0 = True && ac
            |otherwise = False


pairs :: [a] -> [(a,a)]
pairs [] = []
pairs (x : []) = []
pairs (x : y : xs) = (x,y) : pairs xs

uncompress :: [Int] -> [Int]
uncompress xs
    |allNats xs  = foldr step base input
    |otherwise = []
        where
            step (x,y) ac = rodrigo (x,y) x ++ ac
                where
                    rodrigo (x,y) 0 = []
                    rodrigo (x,y) ac = y : rodrigo (x,y) (ac - 1)
            base = []
            input = pairs xs

takeWhiler :: (a -> Bool) -> [a] -> [a]
takeWhiler f = foldr step []
    where
        step x ac
            |f x = x : ac
            |otherwise = []

all3 :: (a -> Bool) -> [a] -> Bool
all3 _ [] = True
all3 f (x : xs)
    |f x = True && all3 f xs
    |otherwise = False

all1 :: (a->Bool)   -> [a] -> Bool
all1 f = foldr step True
    where
        step x ac
            |f x = True && ac
            |otherwise = False

concatMap :: (a -> [b]) -> [a] -> [b]
concatMap _ [] = []
concatMap f (x : xs) = f x ++ concatMap f xs

concatMap' :: (a -> [b]) -> [a] -> [b]
concatMap' f = foldr step []
    where
        step x ac = f x ++ ac


capitalize :: String -> String
capitalize = map toUpper

capitalize' :: String -> String
capitalize' [] = []
capitalize' (x:xs) = toUpper x : capitalize' xs

withoutPrimes :: [Int] -> [Int]
withoutPrimes = filter (not.isPrime) 
    where
        isPrime x 
            |(length [n | n <- [1..x], mod x n == 0]) == 2 = True
            |otherwise = False

withoutPrimes' :: [Int] -> [Int]
withoutPrimes' = foldr step []
    where
        step x ac
            |(not.isPrime) x = x : ac
            |otherwise = ac
                where
                    isPrime x
                        |(length[n | n <- [1..x], mod x n == 0]) == 2 = True
                        |otherwise = False


data Point = Point Float Float
data Shape = Rectangle Point Float Float|
             Circle Point Float|
             Triangle Point Point Point Float

dist :: Point -> Point -> Float
dist (Point x y) (Point x1 y1) = sqrt((x -x1)^2 + (y-y1)^2)

area :: Shape -> Float
area (Rectangle p x y) = x * y
area (Circle p r) = pi * r ^2
area (Triangle p1 p2 p3 h) = (dist p1 p2 * h) / 2