module Main where

import Graphics.Gloss.Interface.Pure.Game
import System.Random
import Tabuleiro
import Peca

-- import Util -- se acharem necessário criar funções que não são específicas pra algo pode criar no módulo

type EstadoJogo = [Peca]

desenharPecas :: EstadoJogo -> Float -> Float -> Float -> Picture
desenharPecas estaPecas tamCel larg altu = pictures (map desenhaPeca pecas)
    where
        pecas = [(x, y, n) | (x, y, n) <- estaPecas]

        desenhaPeca (x, y, n) = pictures [pecaQuadrada, valor] -- funcao local, nao presica cabecalho por conta da inferencia de tipos
            where
            pecaQuadrada = translate posX posY (color red (rectangleSolid (tamCel * 0.8) (tamCel * 0.8)))
            valor = translate (posX-10) (posY-15) (color white (scale 0.3 0.3 (text (show n))))

            posX = fromIntegral x * tamCel - larg / 2 + tamCel / 2
            posY = fromIntegral y * tamCel - altu / 2 + tamCel / 2

desenharTabuleiro :: Picture -- retorna uma Picture quadriculada que representa o desenharTabuleiro
desenharTabuleiro = pictures (map desenhaCelula celulas)
    where
    celulas = [(x, y) | x <- [0..tamanhoGrade-1], y <- [0..tamanhoGrade-1]]

    desenhaCelula (x, y) = pictures [celula, borda]
            where
            celula = translate posX posY (color (greyN 0.5) (rectangleSolid tamCelula tamCelula))
            borda  = translate posX posY (color (greyN 0.8) (rectangleWire tamCelula tamCelula))

            posX = fromIntegral x * tamCelula - larguraTab / 2 + tamCelula / 2
            posY = fromIntegral y * tamCelula - alturaTab / 2 + tamCelula / 2
            tamCelula = tamanhoCelula larguraTab tamanhoGrade


desenharJogo :: EstadoJogo -> Picture -- retorna o estado atual do jogo
desenharJogo estado = pictures [desenharTabuleiro, desenharPecas estado (tamanhoCelula larguraTab tamanhoGrade) larguraTab alturaTab]

moverPeca :: Peca -> Int -> Int -> Peca
moverPeca (x, y, n) alvoX alvoY = if alvoX == x -- movimento em Y
                                    then (x, alvoY, n)
                                    else (alvoX, y, n) -- movimento em X

mover :: Peca -> Event -> EstadoJogo -> EstadoJogo
mover novaPeca (EventKey (SpecialKey KeyUp)    Down _ _) eJogo = -- tecla setinha pra cima
        let novoEJogo = map (\(x, y, n) -> moverPeca (x, y, n) x (tamanhoGrade-1)) eJogo
            in novoEJogo ++ [novaPeca]
mover novaPeca (EventKey (SpecialKey KeyDown)  Down _ _) eJogo = -- tecla setinha pra baixo
    let novoEJogo = map (\(x, y, n) -> moverPeca (x, y, n) x 0) eJogo
        in novoEJogo ++ [novaPeca]
mover novaPeca (EventKey (SpecialKey KeyRight) Down _ _) eJogo = -- tecla setinha pra direita
    let novoEJogo = map (\(x, y, n) -> moverPeca (x, y, n) (tamanhoGrade-1) y) eJogo
        in novoEJogo ++ [novaPeca]
mover novaPeca (EventKey (SpecialKey KeyLeft)  Down _ _) eJogo = -- tecla setinha pra esquerda
    let novoEJogo = map (\(x, y, n) -> moverPeca (x, y, n) 0  y) eJogo
        in novoEJogo ++ [novaPeca]
mover _ _ eJogo = eJogo -- ignora resto do teclado e não faz nada

janela :: Display
janela = InWindow "Tabuleiro" (truncate larguraTab, truncate alturaTab) (0, 0)

main :: IO ()
main = do
    gen <- newStdGen
    let novaPeca = geraPeca gen 0 (tamanhoGrade-1)

    let estadoInicial = [novaPeca]

    play janela white 60 estadoInicial desenharJogo (mover novaPeca) (const id)
