module Main where

import Graphics.Gloss.Interface.IO.Game (playIO)
import Graphics.Gloss.Interface.Pure.Game
import Tabuleiro
import Peca


corPeca :: Color
corPeca = makeColor 0.721568627 0.007843137 0.007843137 1

desenharPeca :: Float -> Float -> Int -> Picture
desenharPeca posX posY valor = 
    let tamanho = tamanhoCelula * 0.8
        in pictures [translate posX posY (color corPeca $ rectangleSolid tamanho tamanho),
                     translate (posX-15) (posY-20) (color white $ scale 0.4 0.4 $ text $ show valor)]

desenharCelula :: Int -> Int -> Int -> Picture
desenharCelula x y valor = 
    let posX = fromIntegral x * tamanhoCelula - tamanhoJanela / 2 + tamanhoCelula / 2
        posY = fromIntegral (tamanhoTabuleiro - 1 - y) * tamanhoCelula - tamanhoJanela / 2 +  tamanhoCelula / 2
        celula = translate posX posY $ color (greyN 0.5) $ rectangleSolid tamanhoCelula tamanhoCelula
        bordaCelula = translate posX posY $ color white $ rectangleWire tamanhoCelula tamanhoCelula
        peca = if valor >= 2 then desenharPeca posX posY valor else blank
    in pictures [celula, bordaCelula, peca]

desenharTabuleiro :: Tabuleiro -> IO Picture
desenharTabuleiro tabuleiro = return $ pictures [desenharCelula x y (tabuleiro !! y !! x) | y <- [0..tamanhoTabuleiro-1], x <- [0..tamanhoTabuleiro-1]]

handleEvent :: Event -> Tabuleiro -> IO Tabuleiro -- esperando moverPeca
handleEvent (EventKey (SpecialKey KeyUp)     Down _ _) tabuleiro = do -- tecla setinha pra cima
    let movido = moverCima tabuleiro
    return $ gerarPecaTabuleiro movido
handleEvent (EventKey (SpecialKey KeyDown)   Down _ _) tabuleiro = do -- tecla setinha pra cima
    let movido = moverBaixo tabuleiro
    return $ gerarPecaTabuleiro movido
handleEvent (EventKey (SpecialKey KeyRight)  Down _ _) tabuleiro = do -- tecla setinha pra direita
    let movido = moverDireita tabuleiro
    return $ gerarPecaTabuleiro movido
handleEvent (EventKey (SpecialKey KeyLeft)   Down _ _) tabuleiro = do -- tecla setinha pra esquerda
    let movido = moverEsquerda tabuleiro
    return $ gerarPecaTabuleiro movido
handleEvent _ tabuleiro = return tabuleiro -- ignora resto do teclado e não faz nada


janela :: Display
janela = InWindow "Tabuleiro" (truncate tamanhoJanela, truncate tamanhoJanela) (0, 0)

main :: IO ()
main = do

    let tabuleiroInicial = alterarElemTabuleiro (tabuleiroVazio tamanhoTabuleiro) x y 2
    
    playIO janela white 60 tabuleiroInicial desenharTabuleiro handleEvent (\_ tabuleiro -> return tabuleiro)
