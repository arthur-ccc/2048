module Peca where

import System.Random (randomRIO)
import Tabuleiro (Tabuleiro, tamanhoTabuleiro)
import Util (transpor)

type Peca = Int

coordenadasVazias :: Tabuleiro -> [(Int, Int)]
coordenadasVazias tabuleiro =
    [(y, x) | y <- [0..tamanhoTabuleiro-1], x <- [0..tamanhoTabuleiro-1], tabuleiro !! y !! x == 0]
    
escolherPosicaoAleatoria :: [(Int, Int)] -> IO (Int, Int)
escolherPosicaoAleatoria vazias = do
    indice <- randomRIO (0, length vazias - 1)
    return (vazias !! indice)

mesclar :: [Peca] -> ([Peca], Int) -- mescla elementos iguais e calcula a pontuação
mesclar [] = ([], 0)
mesclar [x] = ([x], 0)
mesclar (x:y:xs)
    | x == y    = let (resto, _) = mesclar xs 
                  in (x + y : resto, x + y) -- mescla a linha, porém consome os zeros e faz ela fica menor
    | otherwise = let (resto, pontuacaoRestante) = mesclar (y:xs)
                  in (x : resto, pontuacaoRestante)

mesclarLinha :: [Peca] -> ([Peca], Int)
mesclarLinha linha = 
    let (mesclada, pontuacao) = mesclar (filter (/= 0) linha)
    in (mesclada ++ replicate (length linha - length mesclada) 0, pontuacao) -- recebe a linha, mescla e preenche com os zeros

mesclarHorizontal :: Tabuleiro -> (Tabuleiro, Int)
mesclarHorizontal = foldl (\(accTab, accPontuacao) linha -> 
                              let (novaLinha, linhaPontuacao) = mesclarLinha linha
                              in (accTab ++ [novaLinha], accPontuacao + linhaPontuacao)
                           ) ([], 0)

moverEsquerda :: Tabuleiro -> (Tabuleiro, Int)
moverEsquerda = mesclarHorizontal 

moverDireita :: Tabuleiro -> (Tabuleiro, Int)
moverDireita tabuleiro = mesclarHorizontal (map reverse tabuleiro)

moverCima :: Tabuleiro -> (Tabuleiro, Int)
moverCima tabuleiro = 
    let (tab, pontuacao) = mesclarHorizontal (transpor tabuleiro)
    in (transpor tab, pontuacao)

moverBaixo :: Tabuleiro -> (Tabuleiro, Int)
moverBaixo tabuleiro = 
    let (tab, pontuacao) = mesclarHorizontal (map reverse (transpor tabuleiro))
    in (transpor tab, pontuacao)
