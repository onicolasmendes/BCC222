orList :: [Bool] -> Bool
orList (False : []) = False
orList (x : xs)
 | x == False = orList xs
 | otherwise = True