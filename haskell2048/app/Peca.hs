module Peca where
import System.Random (randomRIO)


type Peca = (Int, Int, Int)

geraPeca :: Int -> Int -> IO Peca
geraPeca i f = do
    x <- randomRIO (i, f)
    y <- randomRIO (i, f)
    return (x, y, 2)