module Main where

import Graphics.Gloss.Interface.IO.Game (playIO)
import Graphics.Gloss.Interface.Pure.Game
import Tabuleiro
import Peca

type Score = Int
type Jogo = (Tabuleiro, Score)

desenharPeca :: Float -> Float -> Peca -> Picture
desenharPeca posX posY valor = 
    let tamanho = tamanhoCelula * 0.8
        in pictures [translate posX posY (color red $ rectangleSolid tamanho tamanho),
                     translate (posX-15) (posY-20) (color white $ scale 0.4 0.4 $ text $ show valor)]

desenharCelula :: Int -> Int -> Peca -> Picture
desenharCelula x y valor = pictures [celula, bordaCelula, peca]
    where 
        posX = fromIntegral x * tamanhoCelula - tamanhoJanela / 2 + tamanhoCelula / 2
        posY = fromIntegral (tamanhoTabuleiro - 1 - y) * tamanhoCelula - tamanhoJanela / 2 + tamanhoCelula / 2

        celula = translate posX posY $ color (greyN 0.5) $ rectangleSolid tamanhoCelula tamanhoCelula
        bordaCelula = translate posX posY $ color white $ rectangleWire tamanhoCelula tamanhoCelula
        
        peca = if valor >= 2 then desenharPeca posX posY valor else blank

-- Função que desenha a pontuação
desenharPontuacao :: Int -> Picture
desenharPontuacao pontuacao = 
    translate posX posY $ scale 0.5 0.5 $ color white $ text ("Score: " ++ show pontuacao)
    where 
        posX = (-tamanhoJanela) / 2 + 20
        posY = tamanhoJanela / 2 - 50

desenharTabuleiro :: Tabuleiro -> Picture
desenharTabuleiro tabuleiro = pictures [desenharCelula x y (tabuleiro !! y !! x) | y <- [0..tamanhoTabuleiro-1], x <- [0..tamanhoTabuleiro-1]]

desenharJogo :: Jogo -> IO Picture
desenharJogo (tabuleiro, pontos) = 
    return $ pictures [desenharTabuleiro tabuleiro, desenharPontuacao pontos]

handleEvent :: Event -> Jogo -> IO Jogo -- Esperando moverPeca e retornando a pontuação
handleEvent (EventKey (SpecialKey KeyUp) Down _ _) (tabuleiro, pontuacao) = do
    let (movido, pontos) = moverCima tabuleiro
    (x, y) <- escolherPosicaoAleatoria $ coordenadasVazias movido -- Escolhe uma posição aleatória depois de mover as peças
    let novoTabuleiro = alterarElemTabuleiro movido x y 2
    return (novoTabuleiro, pontuacao + pontos)
handleEvent (EventKey (SpecialKey KeyDown) Down _ _) (tabuleiro, pontuacao) = do
    let (movido, pontos) = moverBaixo tabuleiro
    (x, y) <- escolherPosicaoAleatoria $ coordenadasVazias movido -- Escolhe uma posição aleatória depois de mover as peças
    let novoTabuleiro = alterarElemTabuleiro movido x y 2
    return (novoTabuleiro, pontuacao + pontos)
handleEvent (EventKey (SpecialKey KeyRight) Down _ _) (tabuleiro, pontuacao) = do
    let (movido, pontos) = moverDireita tabuleiro
    (x, y) <- escolherPosicaoAleatoria $ coordenadasVazias movido -- Escolhe uma posição aleatória depois de mover as peças
    let novoTabuleiro = alterarElemTabuleiro movido x y 2
    return (novoTabuleiro, pontuacao + pontos)
handleEvent (EventKey (SpecialKey KeyLeft) Down _ _) (tabuleiro, pontuacao) = do
    let (movido, pontos) = moverEsquerda tabuleiro
    (x, y) <- escolherPosicaoAleatoria $ coordenadasVazias movido -- Escolhe uma posição aleatória depois de mover as peças
    let novoTabuleiro = alterarElemTabuleiro movido x y 2
    return (novoTabuleiro, pontuacao + pontos)
handleEvent _ estado = return estado -- Ignora o restante do teclado e não faz nada

janela :: Display
janela = InWindow "2048" (truncate tamanhoJanela, truncate tamanhoJanela) (0, 0)

main :: IO ()
main = do
    let novo = tabuleiroVazio tamanhoTabuleiro

    (x, y) <- escolherPosicaoAleatoria $ coordenadasVazias novo 

    let tabuleiroInicial = alterarElemTabuleiro novo x y 2
    
    -- Inicializa o jogo com pontuação 0
    playIO janela white 60 (tabuleiroInicial, 0) desenharJogo handleEvent (\_ (tabuleiro, pontuacao) -> return (tabuleiro, pontuacao))
