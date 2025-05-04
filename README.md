# 📦 List Compressor in Haskell

This project uses Haskell to implement a **list compressor and decompressor** for consecutive characters. The method used is **Run-Length Encoding (RLE)**, a simple and lossless compression technique.

---

## 🔧 Features

- **Compression**: transforms a list like `['a','a','a','b','b','c']` into `[(3,'A'), (2,'B'), (1,'C')]`, grouping consecutive repetitions.
- **Decompression**: rebuilds the original list from the compressed format.
- All characters are converted to **uppercase** during compression for standardization.

---

## 📁 Code Structure

### compactar_lista

```haskell
compactar_lista :: [Char] -> [(Int, Char)]
compactar_lista [] = []
compactar_lista (x:xs) =
    (length (x : takeWhile (==x) xs), toUpper x) : compactar_lista (dropWhile (==x) xs)
```

### descompactar_lista

```haskell
descompactar_lista :: [(Int, Char)] -> [Char]
descompactar_lista [] = []
descompactar_lista ((n, c):xs) = replicate n c ++ descompactar_lista xs
```

### main

```haskell
main :: IO ()
main = do
    let originalList = "aaaaaggggtttaaacccccc"
    putStrLn $ "Original list: " ++ show originalList

    let compressed = compactar_lista originalList
    putStrLn $ "Compressed list: " ++ show compressed

    let decompressed = descompactar_lista compressed
    putStrLn $ "Decompressed list: " ++ show decompressed
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
Original list: "aaaaaggggtttaaacccccc"
Compressed list: [(5,'A'),(4,'G'),(3,'T'),(3,'A'),(6,'C')]
Decompressed list: "AAAAAGGGGTTTAAACCCCCC"
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

- [Davi Alves](https://github.com/davialves1820)
- [Gabriel Ribeiro](https://github.com/gabrielbribeiroo)
- [João Vitor](https://github.com/JoaoVitorSampaio)
- [Ryan Caetano](https://github.com/xxxxxx)

---

## 🛠️ Future Suggestions

- Allow user input via the terminal
- Extend to generic list compression (not just `Char`)
- Add tests using `QuickCheck`

---

## 📄 License

This project is licensed under the **MIT License**. See the [LICENSE](LICENSE) file for details.

Course: Functional Programming in Haskell
