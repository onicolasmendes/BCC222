concatMapFoldr :: (a -> [b]) -> [a] ->[b]
concatMapFoldr func = foldr step []
    where
        step x ac = func x ++ ac