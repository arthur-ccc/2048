module Main where

import Graphics.Gloss.Interface.Pure.Game
import System.Random
import Tabuleiro
import Peca

-- import Util

estadoJogo :: [(Int, Int, Int)] -- representa as peças no tabuleiro (posição X, posição Y, valor)
estadoJogo = [(0, 0, 2)] -- estado inicial !!! possivelmente vamos alterar dps para que a primeira peça tbm seja aleatória !!!

renderizar :: [(Int, Int, Int)] -> Picture -- retorna o estado atual do jogo
renderizar estado = pictures [desenhaTabuleiro, desenhaPecas estado (tamanhoCelula largura tamanhoGrade) largura altura]

mover :: (Int, Int, Int) -> Event -> [(Int, Int, Int)] -> [(Int, Int, Int)]
mover novaPeca (EventKey (SpecialKey KeyUp)    Down _ _) estado = -- tecla setinha pra cima
    let (x, _, n) = last estado
        novaPosicao = (x, tamanhoGrade-1, n) -- > aplicar para todas as peças do tabuleiro !!! consertar função ta bugada !!!
        in tail estado ++ [novaPosicao, novaPeca]
mover novaPeca (EventKey (SpecialKey KeyDown)  Down _ _) estado = -- tecla setinha pra baixo
    let (x, _, n) = last estado
        novaPosicao = (x, 0, n)
        in tail estado ++ [novaPosicao, novaPeca]
mover novaPeca (EventKey (SpecialKey KeyRight) Down _ _) estado = -- tecla setinha pra direita
    let (_, y, n) = last estado
        novaPosicao = (tamanhoGrade-1, y, n)
        in tail estado ++ [novaPosicao, novaPeca]
mover novaPeca (EventKey (SpecialKey KeyLeft)  Down _ _) estado = -- tecla setinha pra esquerda
    let (_, y, n) = last estado
        novaPosicao = (0, y, n)
        in tail estado ++ [novaPosicao, novaPeca]
mover _ _ estado = estado -- ignora resto do teclado

janela :: Display
janela = InWindow "Tabuleiro" (truncate largura, truncate altura) (0, 0)

main :: IO ()
main = do
    gen <- newStdGen
    let novaPeca = geraPeca gen 0 (tamanhoGrade-1)

    play janela white 30 estadoJogo renderizar (mover novaPeca) (const id)
