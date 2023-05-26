removeAll :: [Int] -> Int -> [Int]
removeAll [] _ = []
removeAll xs x = [n | n<-xs, n /= x]