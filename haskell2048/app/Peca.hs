module Peca where

import System.Random (randomRIO)
import Tabuleiro (Tabuleiro, tamanhoTabuleiro)
import Util (transpor)
import Graphics.Gloss (Color, makeColor, blue, greyN)

type Peca = Int

coordenadasVazias :: Tabuleiro -> [(Int, Int)]
coordenadasVazias tabuleiro =
    [(y, x) | y <- [0..tamanhoTabuleiro-1], x <- [0..tamanhoTabuleiro-1], tabuleiro !! y !! x == 0]
    
escolherPosicaoAleatoria :: [(Int, Int)] -> IO (Int, Int)
escolherPosicaoAleatoria vazias = do
    indice <- randomRIO (0, length vazias - 1)
    return (vazias !! indice)

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
    | valor `mod` 2048 == 2    = greyN 0.8 -- cinza claro
    | valor `mod` 2048 == 4    = greyN 0.5 -- cinza escuro
    | valor `mod` 2048 == 8    = makeColor 0.9215686274509803 0.6941176470588235 0.30196078431372547 1 -- laranja claro
    | valor `mod` 2048 == 16   = makeColor 0.9411764705882353 0.5333333333333333 0.14901960784313725 1 -- laranja escuro
    | valor `mod` 2048 == 32   = makeColor 0.9490196078431372 0.3960784313725490 0.28627450980392155 1 -- salmão?
    | valor `mod` 2048 == 64   = makeColor 0.9411764705882353 0.2039215686274509 5.49019607843137250 1 -- fúxia
    | valor `mod` 2048 == 128  = makeColor 0.9803921568627451 0.7764705882352941 0.25490196078431370 1 -- amarelo claro
    | valor `mod` 2048 == 256  = makeColor 0.9686274509803922 0.7254901960784313 0.10588235294117647 1 -- amarelo
    | valor `mod` 2048 == 512  = makeColor 0.1333333333333333 0.8901960784313725 0.34901960784313724 1 -- verde lima
    | valor `mod` 2048 == 1024 = makeColor 0.0007843137254901 0.6196078431372549 0.00274509803921568 1 -- verde escuro
    | otherwise = blue
