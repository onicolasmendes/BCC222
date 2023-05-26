indexOf :: [Int] -> Int -> Int -> Int
indexOf [] _ _ = -1
indexOf (x:xs) n ac
 |x /= n = incAc + indexOf xs n ac
 |otherwise = ac +
 