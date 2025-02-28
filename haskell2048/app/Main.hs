module Main where

import Graphics.Gloss.Interface.IO.Game (playIO)
import Graphics.Gloss.Interface.Pure.Game
import Tabuleiro
import Peca
import Util -- se acharem necessário criar funções que não são específicas pra algo, adicionem nesse módulo módulo


type Peca = Int
type EstadoJogo = [[Peca]]

desenharPecas :: EstadoJogo -> Float -> Float -> Float -> Picture
desenharPecas estaPecas tamCel larg altu = pictures (map desenhaPeca pecas)
    where
        pecas = filter (\(_, _, n) -> n>=2) (matrizTLista estaPecas)

        desenhaPeca (x, y, n) = pictures [pecaQuadrada, valor] -- funcao local, nao presica cabecalho por conta da inferencia de tipos
            where
            pecaQuadrada = translate posX posY (color red (rectangleSolid (tamCel * 0.8) (tamCel * 0.8)))
            valor = translate (posX-10) (posY-15) (color white (scale 0.3 0.3 (text (show n))))

            posX = fromIntegral x * tamCel - larg / 2 + tamCel / 2
            posY = fromIntegral y * tamCel - altu / 2 + tamCel / 2

desenharTabuleiro :: Picture -- retorna uma Picture quadriculada que representa o Tabuleiro
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

desenharJogo :: EstadoJogo -> IO Picture -- retorna o estado atual do jogo
desenharJogo estado = return (pictures [desenharTabuleiro, desenharPecas estado (tamanhoCelula larguraTab tamanhoGrade) larguraTab alturaTab])

moverPeca :: EstadoJogo -> EstadoJogo -- função em remodelamento
moverPeca estado = undefined

-- Função auxiliar para mesclar elementos adjacentes iguais
mescla :: [Int] -> [Int]
mescla [] = []
mescla [x] = [x]
mescla (x:y:xs)
    | x == y = (x + y) : mescla xs
    | otherwise = x : mescla (y : xs)

handleEvent :: Event -> EstadoJogo -> IO EstadoJogo -- esperando moverPeca
handleEvent (EventKey (SpecialKey KeyUp)    Down _ _) eJogo = return undefined -- tecla setinha pra cima
handleEvent (EventKey (SpecialKey KeyDown)   Down _ _) eJogo = return undefined -- tecla setinha pra cima
handleEvent (EventKey (SpecialKey KeyRight) Down _ _) eJogo = return undefined -- tecla setinha pra direita
handleEvent (EventKey (SpecialKey KeyLeft)  Down _ _) eJogo = return undefined -- tecla setinha pra esquerda
handleEvent _ eJogo = return eJogo -- ignora resto do teclado e não faz nada

janela :: Display
janela = InWindow "Tabuleiro" (truncate larguraTab, truncate alturaTab) (0, 0)

main :: IO ()
main = do
    (x, y) <- coordenadasAleatorias 0 (tamanhoGrade-1)

    let tabuleiroInicial = alterarElemTabuleiro (tabuleiroVazio tamanhoGrade) x y 2
    
    playIO janela white 60 tabuleiroInicial desenharJogo (\_ b-> return b) (\_ estado -> return estado)
