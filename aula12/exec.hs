data Toy a b = Output a b | Bell b | Done
--Kind: * -> * -> *

instance Functor (Toy a) where
    fmap _ Done = Done
    fmap f (Bell x) = Bell (f x)
    fmap f (Output a b) = Output a (f b)
