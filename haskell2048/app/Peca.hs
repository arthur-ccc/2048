module Peca where


import System.Random (StdGen, Random(randomR))

type Peca = (Int, Int, Int)

geraPeca :: StdGen -> Int -> Int -> (Int, Int, Int) -- função que gera as posições aleatórias, a peça sai com o valor default de 2
geraPeca gen i f =
    let (x, gen')  = randomR (i, f) gen
        (y, gen'') = randomR (i, f) gen'
        in (x, y, 2)
