-- Importação da função para transformar um caractere minúsculo em maiúsculo
import Data.Char (toUpper)

-- Função para compactar lista
-- Compacta uma lista de caracteres consecutivos iguais em pares do tipo (quantidade, caractere)
compactar_lista :: [Char] -> [(Int, Char)] 
compactar_lista [] = []
compactar_lista (x:xs) = (length (x : takeWhile (==x) xs), toUpper x) : compactar_lista (dropWhile (==x) xs)

-- Função para descompactar a lista
-- Reconstrói a lista original a partir da versão compactada
-- Cria uma lista com n repetições do caractere c e concatena com o resultado da descompactação do restante da lista
descompactar_lista :: [(Int, Char)] -> [Char]
descompactar_lista [] = []
descompactar_lista ((n, c):xs) = replicate n c ++ descompactar_lista xs
