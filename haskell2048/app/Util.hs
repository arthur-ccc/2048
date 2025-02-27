module Util where

import System.Random (randomRIO)

coordenadasAleatorias :: Int -> Int -> IO (Int, Int)
coordenadasAleatorias i f = do
    x <- randomRIO (i, f)
    y <- randomRIO (i, f)
    return (x, y)

matrizTLista :: [[Int]] -> [(Int, Int, Int)]
matrizTLista matriz = [(x, y, n) | (x, linha) <- zip [0..] matriz, (y, n) <- zip [0..] linha]

validaMov :: (Int, Int) -> (Int, Int) -> Bool
validaMov (alvoX, alvoY) (limiteInf, limiteSup) = alvoX >= limiteInf && alvoX <= limiteSup 
                                               && alvoY >= limiteInf && alvoY <= limiteSup 
