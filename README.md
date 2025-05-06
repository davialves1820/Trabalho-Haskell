
# 📦 List Compressor in Haskell

This project uses Haskell to implement a **list compressor and decompressor** for consecutive characters. The method used is **Run-Length Encoding (RLE)**, a simple and lossless compression technique.

---

## 🔧 Features

- **Compression**: transforms a list like `['a','a','a','b','b','c']` into `[(3,'A'), (2,'B'), (1,'C')]`, grouping consecutive repetitions.
- **Decompression**: rebuilds the original list from the compressed format.
- All characters are converted to **uppercase** during compression for standardization.
- Supports **interactive user input**.

---

## 📁 Code Structure

### `compactar_lista`

```haskell
compactar_lista :: [Char] -> [(Int, Char)]
compactar_lista [] = []
compactar_lista (x:xs) =
    (length (x : takeWhile (==x) xs), toUpper x) : compactar_lista (dropWhile (==x) xs)
```

### `descompactar_lista`

```haskell
descompactar_lista :: [(Int, Char)] -> [Char]
descompactar_lista [] = []
descompactar_lista ((n, c):xs) = replicate n c ++ descompactar_lista xs
```

### `main`

```haskell
import Data.Char (toUpper)
import System.IO

compactar_lista :: [Char] -> [(Int, Char)] 
compactar_lista [] = []
compactar_lista (x:xs) = (length (x : takeWhile (==x) xs), toUpper x) : compactar_lista (dropWhile (==x) xs)

descompactar_lista :: [(Int, Char)] -> [Char]
descompactar_lista [] = []
descompactar_lista ((n, c):xs) = replicate n c ++ descompactar_lista xs

leitura :: IO String
leitura = do 
    x <- getChar
    if x == '\n' then
       return []
    else do 
        xs <- leitura
        return (x:xs)

main :: IO ()
main = do 
    putStr "Enter a string: "
    xs <- leitura

    let original = xs
    putStrLn ("Original list: " ++ show original)

    let compactada = compactar_lista original
    putStrLn $ "Compressed list: " ++ show compactada

    let descompactada = descompactar_lista compactada
    putStrLn $ "Decompressed list: " ++ show descompactada
```

---

## ▶️ How to Run

### Prerequisites

- Install [GHC and GHCi](https://www.haskell.org/platform/) (recommended: GHC 9.4+)

### Steps

1. Save the code to a file named `Compressor.hs`
2. Run using GHCi:

```bash
ghci Compressor.hs
main
```

Or directly with:

```bash
runghc Compressor.hs
```

---

## 📝 Sample Output

```
Enter a string: aaaabbbcccc
Original list: "aaaabbbcccc"
Compressed list: [(4,'A'),(3,'B'),(4,'C')]
Decompressed list: "AAAABBBCCCC"
```

---

## 🧠 Learning Outcomes

This exercise is a great way to learn about:
- Recursion over lists
- Higher-order functions (`takeWhile`, `dropWhile`)
- Character manipulation using `toUpper`
- Basic I/O in Haskell

---

## 📌 Authors

<div align="center" style="display: flex; gap: 20px; align-items: center; flex-wrap: wrap;">
  <a href="https://github.com/davialves1820" target="_blank">
    <img src="https://github.com/davialves1820.png" width="100" style="border-radius: 50%;" alt="Davi Alves"/>
  </a>
  <a href="https://github.com/gabrielbribeiroo" target="_blank">
    <img src="https://github.com/gabrielbribeiroo.png" width="100" style="border-radius: 50%;" alt="Gabriel Ribeiro"/>
  </a>      
  <a href="https://github.com/JoaoVitorSampaio" target="_blank">
    <img src="https://github.com/JoaoVitorSampaio.png" width="100" style="border-radius: 50%;" alt="Joao Vitor"/>
  </a>
  <a href="https://github.com/Ryan1804" target="_blank">
    <img src="https://github.com/Ryan1804.png" width="100" style="border-radius: 50%;" alt="Ryan Caetano"/>
  </a>
</div>

---

## 🛠️ Future Suggestions

- Allow user input via the terminal
- Extend to generic list compression (not just `Char`)
- Add tests using `QuickCheck`

---

## 📄 License

This project is licensed under the **MIT License**. See the [LICENSE](LICENSE) file for details.

Course: Functional Programming in Haskell
