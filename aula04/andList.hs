andList :: [Bool] -> Bool
andList (True: []) = True
andList (x : xs)
 | x == True = andList (xs)
 | otherwise = False