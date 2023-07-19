--Exercicio
import Data.List


insereOrdenado :: Int -> [Int] -> [Int]
insereOrdenado x [] = [x]
insereOrdenado n (x:xs) = if n <= x then (n : x :xs) else x : insereOrdenado n xs
           
generateList :: [Int]
generateList = generate [1,2,3] []
   
generate :: [Int] -> [Int] -> [Int]
generate [] ac = ac
generate (x:xs) ns = generate xs (insereOrdenado x (insereOrdenado (5*x) (insereOrdenado (3*x) (insereOrdenado (2*x) ns))))