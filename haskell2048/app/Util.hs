module Util where

import System.Random (randomRIO)


coordenadasAleatorias :: Int -> Int -> IO (Int, Int)
coordenadasAleatorias i f = do
    x <- randomRIO (i, f)
    y <- randomRIO (i, f)
    return (x, y)

validaMov :: (Int, Int) -> (Int, Int) -> Bool
validaMov (alvoX, alvoY) (limiteInf, limiteSup) = alvoX >= limiteInf && alvoX <= limiteSup 
                                               && alvoY >= limiteInf && alvoY <= limiteSup 

matrizTLista :: [[Int]] -> [(Int, Int, Int)]
matrizTLista matriz = [(x, y, n) | (x, linha) <- zip [0..] matriz, (y, n) <- zip [0..] linha]

alterarElemLista :: [Int] -> Int -> Int -> [Int]
alterarElemLista lista i novoValor = take i lista ++ [novoValor] ++ drop (i+1) lista

transpor :: [[a]] -> [[a]]
transpor ([]:_) = []
transpor x = map head x : transpor (map tail x)
