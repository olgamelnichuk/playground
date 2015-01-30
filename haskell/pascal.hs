-- printing out pascal triangle    

module Main where

pascal n = printItOut (pascal' n)

pascal' :: Int -> [[Int]]
pascal' 1 = [[1]]
pascal' n = a ++ [nextLevel ([0] ++ (last a) ++ [0])]
     where a = pascal' (n-1)
     
nextLevel [x] = [] 
nextLevel (x:xs) = [x + head xs] ++ nextLevel xs

printItOut :: [[Int]] -> IO ()
printItOut xss = putStr (unlines [showLine x | x <- xss])

showLine xs = foldl (\acc y -> acc ++ (show y) ++ " ") "" xs

main = do
    input <- getLine
    pascal . (read :: String -> Int) $ input
