
-- Exercicio 1
data Vec3 = Vec3 Int Int Int
            deriving (Show)
            
instance Eq Vec3 where
    (Vec3 a b c) == (Vec3 d e f) = (a == d) && (b == e) && (c == f)
    n /= m = not (n == m)

verificaVec3 :: Vec3 -> Vec3 -> Bool
verificaVec3 x y = x == y

-- Exercicio 2
data Person = Person {
        name :: String
        , age :: Int       
}
instance Eq Person where
    (Person name age) == (Person name1 age1) = name == name1
    p1 /= p2 = not (p1 == p2)

verificaPerson :: Person -> Person -> Bool
verificaPerson p1 p2 = p1 == p2

-- Exercicio 3
-- show :: a -> String

instance Show Person where
    show (Person name age)  = name

showPerson :: Person -> String
showPerson p = show p