import Data.Char (toUpper)

-- Função para compactar lista
compactar_lista :: [Char] -> [(Int, Char)]
compactar_lista [] = []
compactar_lista (x:xs) = (length (x : takeWhile (==x) xs), toUpper x) : compactar_lista (dropWhile (==x) xs)
