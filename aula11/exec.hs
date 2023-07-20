import System.IO  

main :: IO ()
main = do
    file <- openFile "text.txt" ReadMode

    content <- hGetContents file

    putStrLn content

    putStrLn("Linhas: " ++ show (contLines content) ++ " Palavras: " ++ show(countWords content))

    hClose file

contLines :: String -> Int
contLines [] = 0
contLines xs = cont xs 1
    where
        cont [] ac = ac
        cont (x:xs) ac
            |x == '\n' = cont xs (ac+1)
            |otherwise = cont xs ac

countWords :: String -> Int
countWords [] = 0
countWords xs = count xs 1
    where
        count [] ac = ac
        count (x:xs) ac
            |(x == '\n') ||(x == ' ') = count xs (ac+1)
            |otherwise = count xs ac