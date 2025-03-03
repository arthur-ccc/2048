module Peca where

import Tabuleiro (Tabuleiro)
import Util (transpor)
import Graphics.Gloss (Color, makeColor)


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

corPeca :: Int -> Color
corPeca valor
    | valor `mod` 2048 == 2    = makeColor 0.98800000000000000 0.3640000000000000000 0.3400000000000000000 1 -- vermelhinho
    | valor `mod` 2048 == 4    = makeColor 0.00000000000000000 0.6880000000000000000 0.7840000000000000000 1 -- rosa                                                               -- cinza
    | valor `mod` 2048 == 8    = makeColor 0.00000000000000000 0.7520000000000000000 0.4240000000000000000 1 -- laranja
    | valor `mod` 2048 == 16   = makeColor 1.00800000000000000 0.9160000000000000000 0.5040000000000000000 1 -- amarelo pastel
    | valor `mod` 2048 == 32   = makeColor 0.82400000000000000 0.9800000000000000000 0.5280000000000000000 1 -- verde clarinho
    | valor `mod` 2048 == 64   = makeColor 0.52800000000000000 0.9640000000000000000 0.4640000000000000000 1 -- verde mais escuro
    | valor `mod` 2048 == 128  = makeColor 0.48000000000000000 0.8640000000000000000 0.6880000000000000000 1 -- verde água
    | valor `mod` 2048 == 256  = makeColor 0.56400000000000000 0.8720000000000000000 1.0000000000000000000 1 -- azul claro
    | valor `mod` 2048 == 512  = makeColor 0.49600000000000000 0.5040000000000000000 1.0000000000000000000 1 -- azul escuro
    | valor `mod` 2048 == 1024 = makeColor 0.78800000000000000 0.4720000000000000000 1.0000000000000000000 1 -- lilás                                                              -- cinza
    | otherwise = makeColor 0.79600000000000000 0.1200000000000000000 1.0000000000000000000 1 -- violeta
