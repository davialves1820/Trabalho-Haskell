import Data.Char (toUpper)

prova :: [Char] -> [(Int, Char)]
prova [] = []
prova (x:xs) = (length (x : takeWhile (==x) xs), toUpper x) : prova (dropWhile (==x) xs)