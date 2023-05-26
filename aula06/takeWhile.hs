takeWhileFoldr :: (a-> Bool) -> [a] -> [a]
takeWhileFoldr f = foldr step []
    where
        step x ac -- X é o elemento da lista e ac é o resultado da chamada recursvia
            |f x = x : ac
            |otherwise = []

takeWhileRecursion :: (a -> Bool) -> [a] -> [a]
takeWhileRecursion _ [] = []
takeWhileRecursion func (x:xs)
    |func x = x : takeWhileRecursion func xs
    |otherwise = []