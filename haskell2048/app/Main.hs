module Main where

import Graphics.Gloss.Interface.IO.Game (playIO)
import Graphics.Gloss.Interface.Pure.Game
import Tabuleiro
import Util


type Peca = Int

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
    (x, y) <- coordenadasAleatorias 0 (tamanhoTabuleiro-1)
    let novoTabuleiro = alterarElemTabuleiro (moverCima tabuleiro) x y 2
    
    return novoTabuleiro
handleEvent (EventKey (SpecialKey KeyDown)   Down _ _) tabuleiro = do -- tecla setinha pra cima
    (x, y) <- coordenadasAleatorias 0 (tamanhoTabuleiro-1)
    let novoTabuleiro = alterarElemTabuleiro (moverBaixo tabuleiro) x y 2
    
    return novoTabuleiro
handleEvent (EventKey (SpecialKey KeyRight)  Down _ _) tabuleiro = do -- tecla setinha pra direita
    (x, y) <- coordenadasAleatorias 0 (tamanhoTabuleiro-1)
    let novoTabuleiro = alterarElemTabuleiro (moverDireita tabuleiro) x y 2
    
    return novoTabuleiro
handleEvent (EventKey (SpecialKey KeyLeft)   Down _ _) tabuleiro = do -- tecla setinha pra esquerda
    (x, y) <- coordenadasAleatorias 0 (tamanhoTabuleiro-1)
    let novoTabuleiro = alterarElemTabuleiro (moverEsquerda tabuleiro) x y 2
    
    return novoTabuleiro
handleEvent _ tabuleiro = return tabuleiro -- ignora resto do teclado e não faz nada

janela :: Display
janela = InWindow "Tabuleiro" (truncate tamanhoJanela, truncate tamanhoJanela) (0, 0)

main :: IO ()
main = do
    (x, y) <- coordenadasAleatorias 0 (tamanhoTabuleiro-1)

    let tabuleiroInicial = alterarElemTabuleiro (tabuleiroVazio tamanhoTabuleiro) x y 2
    
    playIO janela white 60 tabuleiroInicial desenharTabuleiro handleEvent (\_ tabuleiro -> return tabuleiro)
