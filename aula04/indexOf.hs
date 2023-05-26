indexOf :: [Int] -> Int -> Int
indexOf xs n = index xs n 0
 where
    index [] _ _ = -1
    index (x : xs) n ac
        |x == n = ac
        |otherwise = index (xs) n (ac + 1)

 