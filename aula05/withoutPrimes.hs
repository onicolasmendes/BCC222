withoutPrimes :: [Int] -> [Int]
withoutPrimes [] = []
withoutPrimes xs = filter prime xs
 where
    prime n 
        |(length [p | p <- [1 .. n], mod n p == 0]) > 2 = True
        |otherwise = False