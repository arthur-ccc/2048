module Tabuleiro where

import System.Random (randomRIO)
import Util


type Tabuleiro = [[Int]]

tamanhoTabuleiro :: Int -- a quantidade de células por linha !!!posteriormente deve ser replanejado para criar diferentes níveis de dificuldade!!!
tamanhoTabuleiro = 4

tamanhoCelula :: Float
tamanhoCelula = 200

tamanhoJanela :: Float
tamanhoJanela = tamanhoCelula * fromIntegral tamanhoTabuleiro

tabuleiroVazio :: Int -> [[Int]]
tabuleiroVazio tamanho = replicate tamanho (replicate tamanho 0)

alterarElemTabuleiro :: [[Int]] -> Int -> Int -> Int -> [[Int]]
alterarElemTabuleiro matriz x y novoValor = take x matriz ++ [alterarElemLista (matriz!!x) y novoValor] ++ drop (x+1) matriz

mesclar :: [Int] -> [Int] -- mescla elementos iguais
mesclar [] = []
mesclar [x] = [x]
mesclar (x:y:xs)
    | x == y = (x + y) : mesclar xs
    | otherwise = x : mesclar (y : xs)

mesclarLinha :: [Int] -> [Int]
mesclarLinha linha = mesclada ++ replicate (length linha - length mesclada) 0
    where
        filtrada = filter (/= 0) linha
        mesclada = mesclar filtrada

mesclarHorizontal :: Tabuleiro -> Tabuleiro
mesclarHorizontal = map mesclarLinha

moverEsquerda :: Tabuleiro -> Tabuleiro
moverEsquerda = mesclarHorizontal

moverDireita :: Tabuleiro -> Tabuleiro
moverDireita tabuleiro = map reverse (mesclarHorizontal (map reverse tabuleiro))

moverCima :: Tabuleiro -> Tabuleiro
moverCima tabuleiro = transpor $ moverEsquerda $ transpor tabuleiro

moverBaixo :: Tabuleiro -> Tabuleiro
moverBaixo tabuleiro = transpor $ moverDireita $ transpor tabuleiro

-- Retorna uma lista de coordenadas (x, y) das células vazias
coordenadasVazias :: Tabuleiro -> [(Int, Int)]
coordenadasVazias tabuleiro =
    [(x, y) | y <- [0..tamanhoTabuleiro-1], x <- [0..tamanhoTabuleiro-1], tabuleiro !! y !! x == 0]


-- Escolhe uma posição aleatória da lista de coordenadas vazias
escolherPosicaoAleatoria :: [(Int, Int)] -> IO (Int, Int)
escolherPosicaoAleatoria vazias = do
    indice <- randomRIO (0, length vazias - 1)
    return (vazias !! indice)