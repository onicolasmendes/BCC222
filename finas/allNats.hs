module AllNats where

allNats :: [Int] -> Bool
allNats = foldr step True
    where
        step x ac 
            |x > 0 = True && ac
            |otherwise = False
            