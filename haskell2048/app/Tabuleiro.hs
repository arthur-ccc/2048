module Tabuleiro where

import Graphics.Gloss.Interface.Pure.Game
import Util (alterarElemLista)

type Tabuleiro = [[Int]]

-- Constantes
tamanhoCelula :: Float
tamanhoCelula = 200

-- Função para calcular o tamanho da janela com base no tamanho do tabuleiro
tamanhoJanela :: Int -> Float
tamanhoJanela tamanho = tamanhoCelula * fromIntegral tamanho

-- Cria um tabuleiro vazio com o tamanho especificado
tabuleiroVazio :: Int -> Tabuleiro
tabuleiroVazio tamanho = replicate tamanho (replicate tamanho 0)

-- Altera o valor de uma célula no tabuleiro
alterarElemTabuleiro :: Tabuleiro -> Int -> Int -> Int -> Tabuleiro
alterarElemTabuleiro matriz x y novoValor = take x matriz ++ [alterarElemLista (matriz!!x) y novoValor] ++ drop (x+1) matriz

-- Renderiza a tela de seleção de dificuldade
desenharTelaDificuldade :: Maybe Int -> Picture
desenharTelaDificuldade escolha = pictures
    [ translate (-200) 100 (scale 0.5 0.5 (color white (text "Escolha a Dificuldade")))
    , translate (-200) 50 (scale 0.3 0.3 (color (corEscolha 4) (text "1 - Facil (4x4)")))
    , translate (-200) 0 (scale 0.3 0.3 (color (corEscolha 5) (text "2 - Medio (5x5)")))
    , translate (-200) (-50) (scale 0.3 0.3 (color (corEscolha 6) (text "3 - Dificil (6x6)")))
    , translate (-200) (-150) (scale 0.2 0.2 (color white (text "Pressione 1, 2 ou 3 para escolher")))
    ]
  where
    corEscolha tamanho = if escolha == Just tamanho then green else white

-- Processa a escolha do jogador e retorna o tamanho do tabuleiro
tamanhoTabuleiro :: Char -> Maybe Int
tamanhoTabuleiro tecla =
    case tecla of
        '1' -> Just 4  -- Fácil
        '2' -> Just 5  -- Médio
        '3' -> Just 6  -- Difícil
        _   -> Nothing -- Escolha inválida
