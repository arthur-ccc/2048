module Main where

import Graphics.Gloss.Interface.IO.Game (playIO)
import Graphics.Gloss.Interface.Pure.Game
import Tabuleiro
import Peca
-- import Util


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

desenharTabuleiro :: (Tabuleiro, Int) -> IO Picture
desenharTabuleiro (tabuleiro, pontuacao) = do
    let tabuleiroDesenhado = pictures [desenharCelula x y (tabuleiro !! y !! x) | y <- [0..tamanhoTabuleiro-1], x <- [0..tamanhoTabuleiro-1]]
    let pontuacaoDesenhada = desenharPontuacao pontuacao
    return $ pictures [tabuleiroDesenhado, pontuacaoDesenhada]

handleEvent :: Event -> (Tabuleiro, Int) -> IO (Tabuleiro, Int) -- Esperando moverPeca e retornando a pontuação
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
janela = InWindow "Tabuleiro" (truncate tamanhoJanela, truncate tamanhoJanela) (0, 0)

-- Função que desenha a pontuação
desenharPontuacao :: Int -> Picture
desenharPontuacao pontuacao = 
    translate (-tamanhoJanela / 2 + 20) (tamanhoJanela / 2 - 50) $
        scale 0.5 0.5 $
        color white $
        text ("Score: " ++ show pontuacao)

main :: IO ()
main = do
    let novo = tabuleiroVazio tamanhoTabuleiro

    (x, y) <- escolherPosicaoAleatoria $ coordenadasVazias novo 

    let tabuleiroInicial = alterarElemTabuleiro novo x y 2
    
    -- Inicializa o jogo com pontuação 0
    playIO janela white 60 (tabuleiroInicial, 0) desenharTabuleiro handleEvent (\_ (tabuleiro, pontuacao) -> return (tabuleiro, pontuacao))

