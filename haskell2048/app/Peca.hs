module Peca where

import Tabuleiro (Tabuleiro)
import Util (transpor)
import Graphics.Gloss (Color, makeColor, blue, red, orange, yellow, green, cyan, white, greyN)


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
    | valor `mod` 2048 == 2    = red
    | valor `mod` 2048 == 4    = makeColor 1.00000000000000000 0.0000000000000000000 0.85098039215686270 1 --rosa
    | valor `mod` 2048 == 8    = orange
    | valor `mod` 2048 == 16   = yellow
    | valor `mod` 2048 == 32   = green
    | valor `mod` 2048 == 64   = makeColor 0.00313725490196070 0.2901960784313726000 0.00039215686274509 1 -- verde escuro
    | valor `mod` 2048 == 128  = cyan
    | valor `mod` 2048 == 256  = blue
    | valor `mod` 2048 == 512  = makeColor 0.43137254901960786 0.0011764705882352941 0.36862745098039220 1 -- roxo
    | valor `mod` 2048 == 1024 = greyN 0.5                                                                 -- cinza
    | otherwise = white
