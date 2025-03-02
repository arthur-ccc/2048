module Tabuleiro where

import Util (alterarElemLista)


type Tabuleiro = [[Int]]

tamanhoTabuleiro :: Int -- a quantidade de células por linha
tamanhoTabuleiro = 4

tamanhoCelula :: Float
tamanhoCelula = 200 -- tamanho em pixels

tamanhoJanela :: Float
tamanhoJanela = tamanhoCelula * fromIntegral tamanhoTabuleiro

tabuleiroVazio :: Int -> Tabuleiro
tabuleiroVazio tamanho = replicate tamanho (replicate tamanho 0)

alterarElemTabuleiro :: Tabuleiro -> Int -> Int -> Int -> Tabuleiro
alterarElemTabuleiro matriz x y novoValor = take x matriz ++ [alterarElemLista (matriz!!x) y novoValor] ++ drop (x+1) matriz
