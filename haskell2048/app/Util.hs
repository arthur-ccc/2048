module Util where

import System.Random (randomRIO)

coordenadasAleatorias :: Int -> Int -> IO (Int, Int)
coordenadasAleatorias i f = do
    x <- randomRIO (i, f-1)
    y <- randomRIO (i, f-1)
    return (x, y)

matrizTLista :: [[Int]] -> [(Int, Int, Int)]
matrizTLista matriz = [(x, y, n) | (x, linha) <- zip [0..] matriz, (y, n) <- zip [0..] linha]

validaMov :: (Int, Int) -> (Int, Int) -> Bool
validaMov (alvoX, alvoY) (limiteInf, limiteSup) = alvoX >= limiteInf && alvoX <= limiteSup 
                                               && alvoY >= limiteInf && alvoY <= limiteSup 

alterarElemLista :: [Int] -> Int -> Int -> [Int]
alterarElemLista lista i novoValor = take i lista ++ [novoValor] ++ drop (i+1) lista


-- Tipo de dados para o estado do jogo (matriz de peças)
type EstadoJogo = [[Int]]

-- Função para mesclar uma linha (horizontalmente)
mesclarLinha :: [Int] -> [Int]
mesclarLinha linha = mesclada ++ replicate (length linha - length mesclada) 0
    where
        -- Filtra os zeros e mescla os elementos
        filtrada = filter (/= 0) linha
        mesclada = mescla filtrada

-- Função para mesclar todas as linhas (horizontalmente)
mesclarHorizontal :: EstadoJogo -> EstadoJogo
mesclarHorizontal = map mesclarLinha

-- Função para transpor uma matriz (transformar colunas em linhas)
transpor :: [[a]] -> [[a]]
transpor ([]:_) = []
transpor x = map head x : transpor (map tail x)

-- Função para mesclar todas as colunas (verticalmente)
mesclarVertical :: EstadoJogo -> EstadoJogo
mesclarVertical tabuleiro = transpor (mesclarHorizontal (transpor tabuleiro))

-- Função para mover o tabuleiro para a esquerda
moverEsquerda :: EstadoJogo -> EstadoJogo
moverEsquerda = mesclarHorizontal

-- Função para mover o tabuleiro para a direita
moverDireita :: EstadoJogo -> EstadoJogo
moverDireita tabuleiro = map reverse (mesclarHorizontal (map reverse tabuleiro))

-- Função para mover o tabuleiro para cima
moverCima :: EstadoJogo -> EstadoJogo
moverCima = mesclarVertical

-- Função para mover o tabuleiro para baixo
moverBaixo :: EstadoJogo -> EstadoJogo
moverBaixo tabuleiro = map reverse (transpor (mesclarHorizontal (transpor (map reverse tabuleiro))))

-- Função para completar com zeros (garantir que todas as linhas/colunas tenham o mesmo tamanho)
completarComZeros :: EstadoJogo -> EstadoJogo
completarComZeros tabuleiro = map completarLinha tabuleiro
    where
        completarLinha linha = linha ++ replicate (tamanho - length linha) 0
        tamanho = maximum (map length tabuleiro)
