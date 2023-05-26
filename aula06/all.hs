allFoldr :: (a -> Bool) -> [a] -> Bool
allFoldr func = foldr step True
    where
        step x ac
         |func x = True && ac
         |otherwise = False

allRecursion :: (a -> Bool) -> [a] -> Bool
allRecursion _ [] = True
allRecursion func (x : xs)
    |func x = True && allRecursion func xs
    |otherwise = False