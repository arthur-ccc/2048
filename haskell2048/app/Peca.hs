module Peca where

import Tabuleiro (Tabuleiro)
import Util (transpor)


type Peca = Int

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
