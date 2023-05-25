-- Ord para tipos que suportam teste de comparação e Num para classe numerica
exitsPositive :: Ord a => Num a => [a] -> Bool
exitsPositive [] = False
exitsPositive (x : xs)
 |x > 0 = True
 |otherwise = exitsPositive xs
