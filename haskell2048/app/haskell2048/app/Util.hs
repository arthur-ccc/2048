module Util where


matrizTLista :: [[Int]] -> [(Int, Int, Int)]
matrizTLista matriz = [(x, y, n) | (x, linha) <- zip [0..] matriz, (y, n) <- zip [0..] linha]

alterarElemLista :: [Int] -> Int -> Int -> [Int]
alterarElemLista lista i novoValor = take i lista ++ [novoValor] ++ drop (i+1) lista

transpor :: [[a]] -> [[a]]
transpor ([]:_) = []
transpor x = map head x : transpor (map tail x)
