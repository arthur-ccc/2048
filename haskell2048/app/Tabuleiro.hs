module Tabuleiro where


tamanhoGrade :: Int -- a quantidade de células por linha !!!posteriormente deve ser replanejado para criar diferentes níveis de dificuldade!!!
tamanhoGrade = 5

larguraTab, alturaTab :: Float -- dimensões do Tabuleiro
larguraTab = 800
alturaTab = 800

tamanhoCelula ::  Float -> Int -> Float -- é o tamnho de cada quadrado em que a peça pode aparecer
tamanhoCelula larg tamGrade = larg / fromIntegral tamGrade
