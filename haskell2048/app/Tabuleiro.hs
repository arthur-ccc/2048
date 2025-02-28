module Tabuleiro where
import Util (alterarElemLista)


tamanhoGrade :: Int -- a quantidade de células por linha !!!posteriormente deve ser replanejado para criar diferentes níveis de dificuldade!!!
tamanhoGrade = 5

larguraTab, alturaTab :: Float -- dimensões do Tabuleiro
larguraTab = 800
alturaTab = 800

tamanhoCelula ::  Float -> Int -> Float -- é o tamnho de cada quadrado em que a peça pode aparecer
tamanhoCelula larg tamGrade = larg / fromIntegral tamGrade

tabuleiroVazio :: Int -> [[Int]]
tabuleiroVazio tamGrade = replicate (tamGrade-1) (replicate (tamGrade-1) 0)

alterarElemTabuleiro :: [[Int]] -> Int -> Int -> Int -> [[Int]]
alterarElemTabuleiro matriz x y novoValor = take x matriz ++ [alterarElemLista (matriz!!x) y novoValor] ++ drop (x-1) matriz
