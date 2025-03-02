module Tabuleiro where

import Util (alterarElemLista)
import System.Random (randomRIO)


type Tabuleiro = [[Int]]

tamanhoCelula :: Float
tamanhoCelula = 200 -- tamanho em pixels

tamanhoJanela :: Int -> Float
tamanhoJanela tamanhoTabuleiro = tamanhoCelula * fromIntegral tamanhoTabuleiro

alterarElemTabuleiro :: Tabuleiro -> Int -> Int -> Int -> Tabuleiro
alterarElemTabuleiro matriz x y novoValor = take x matriz ++ [alterarElemLista (matriz!!x) y novoValor] ++ drop (x+1) matriz

tabuleiroVazio :: Int -> Tabuleiro
tabuleiroVazio tamanho = replicate tamanho (replicate tamanho 0)

inserirPecaNova :: Tabuleiro -> Int -> Int -> Tabuleiro
inserirPecaNova tabuleiro x y = alterarElemTabuleiro tabuleiro x y 2

coordenadasVazias :: Tabuleiro -> [(Int, Int)]
coordenadasVazias tabuleiro =
    [(y, x) | y <- [0..tam-1], x <- [0..tam-1], tabuleiro !! y !! x == 0]
    where
        tam = length tabuleiro

temCoordenadaLivre :: Tabuleiro -> Bool
temCoordenadaLivre tabuleiro = not $ null (coordenadasVazias tabuleiro)

escolherPosicaoAleatoria :: Tabuleiro -> IO (Int, Int)
escolherPosicaoAleatoria tabuleiro = do
    let vazias = coordenadasVazias tabuleiro
    indice <- randomRIO (0, length vazias - 1)
    return (vazias !! indice)

escolhaTabuleiro :: Char -> Maybe Int
escolhaTabuleiro opcao 
    | opcao == '1' = Just 4 -- Fácil
    | opcao == '2' = Just 5 -- Média
    | opcao == '3' = Just 6 -- Difícil
    | otherwise = Nothing   -- Tecla inválida
