module Tabuleiro where

import Graphics.Gloss

tamanhoGrade :: Int -- a quantidade de células por linha obs: posteriormente deve ser replanejado para criar diferentes níveis de dificuldade
tamanhoGrade = 5

largura, altura :: Float -- dimensões do desenhaTabuleiro
largura = 800
altura = 800

tamanhoCelula ::  Float -> Int -> Float -- é o tamnho de cada quadrado em que a peça pode aparecer
tamanhoCelula larg tamGrade = larg / fromIntegral tamGrade

desenhaTabuleiro :: Picture -- retorna uma Picture quadriculada que representa o desenhaTabuleiro
desenhaTabuleiro = pictures (map desenhaCelula celulas)
    where
    celulas = [(x, y) | x <- [0..tamanhoGrade-1], y <- [0..tamanhoGrade-1]]

    desenhaCelula :: (Int, Int) -> Picture
    desenhaCelula (x, y) = pictures [celula, borda]
            where
            celula = translate posX posY (color (greyN 0.5) (rectangleSolid tamCelula tamCelula))
            borda  = translate posX posY (color (greyN 0.8) (rectangleWire tamCelula tamCelula))

            posX = fromIntegral x * tamCelula - largura / 2 + tamCelula / 2
            posY = fromIntegral y * tamCelula - altura / 2 + tamCelula / 2
            tamCelula = tamanhoCelula largura tamanhoGrade
