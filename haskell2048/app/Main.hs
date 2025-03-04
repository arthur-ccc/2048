module Main where

import Graphics.Gloss.Interface.IO.Game (playIO)
import Graphics.Gloss.Interface.Pure.Game
import Tabuleiro
import Peca


data Jogo = HUB | Dificuldade | Partida Tabuleiro Int | GameOver Int | Vitoria Int

desenharHUB :: Picture
desenharHUB = pictures
    [ translate (-100) 50    (scale 0.5 0.5 (color white (text "2048")))
    , translate (-200) (-50) (scale 0.2 0.2 (color white (text "Pressione ENTER para jogar")))
    ]

desenharTelaDificuldade :: Picture
desenharTelaDificuldade = pictures [
      translate (-300) 100    (scale 0.5 0.5 (color white  (text "Escolha a Dificuldade")))
    , translate (-200) 50     (scale 0.3 0.3 (color green  (text "1 - Facil (4x4)")))
    , translate (-200) 0      (scale 0.3 0.3 (color yellow (text "2 - Medio (5x5)")))
    , translate (-200) (-50)  (scale 0.3 0.3 (color red    (text "3 - Dificil (6x6)")))
    , translate (-250) (-150) (scale 0.2 0.2 (color white  (text "Pressione 1, 2 ou 3 para escolher")))
    ]

desenharPeca :: Int -> Float -> Float -> Peca -> Picture
desenharPeca tamanhoJogo posX posY valor =
    let tamanho = tamanhoCelula tamanhoJogo * 0.8
        in pictures [translate posX posY (color (corPeca valor) $ rectangleSolid tamanho tamanho),
                     translate (posX-15) (posY-20) (color white $ scale 0.4 0.4 $ text $ show valor)]

desenharCelula :: Int -> Int -> Int -> Peca -> Picture -- ajustar para as células diminuires de acordo com o tamanho
desenharCelula tamanhoJogo x y valor = pictures [celula, bordaCelula, peca]
    where
        posX = fromIntegral x * tamanhoCelula tamanhoJogo - tamanhoTabuleiroPixels / 2 + tamanhoCelula tamanhoJogo / 2
        posY = fromIntegral (tamanhoJogo - 1 - y) * tamanhoCelula tamanhoJogo - tamanhoTabuleiroPixels / 2 + tamanhoCelula tamanhoJogo / 2

        celula = translate posX posY $ color (greyN 0.3) $ rectangleSolid (tamanhoCelula tamanhoJogo) (tamanhoCelula tamanhoJogo)
        bordaCelula = translate posX posY $ color white $ rectangleWire (tamanhoCelula tamanhoJogo) (tamanhoCelula tamanhoJogo)

        peca = if valor >= 2 then desenharPeca tamanhoJogo posX posY valor else blank

desenharTabuleiro :: Tabuleiro -> Picture
desenharTabuleiro tabuleiro = pictures [desenharCelula (length tabuleiro) x y (tabuleiro !! y !! x) | y <- [0..length tabuleiro - 1], x <- [0..length tabuleiro - 1]]

desenharPontuacao :: Int -> Picture
desenharPontuacao pontuacao =
    translate posX posY $ scale 0.4 0.4 $ color white $ text ("Score: " ++ show pontuacao)
    where
        posX = 50 + tamanhoTabuleiroPixels / 2
        posY = tamanhoTabuleiroPixels / 2 - 50

desenharPartida :: Tabuleiro -> Int -> Picture
desenharPartida tabuleiro pontuacao = pictures [desenharTabuleiro tabuleiro, desenharPontuacao pontuacao] -- !!! modificar desenharTabuleiro !!!

desenharGameOver :: Picture
desenharGameOver =
    let 
        textoGameOver = scale 0.6 0.6 $ text "GAME OVER"
        corTexto = color red
        deslocamentos = [(dx, dy) | dx <- [-2,0,2], dy <- [-2,0,2]]
        textoGrosso = pictures [translate dx dy (corTexto textoGameOver) | (dx, dy) <- deslocamentos]
    in pictures [
        translate (-225) 100 textoGrosso,
        translate (-275) 0 $ color white $ scale 0.3 0.3 $ text "Pressione ENTER para reiniciar"
    ]

desenharVitoria :: Picture
desenharVitoria =
    let 
        fundo = color (makeColorI 255 215 0 1) (rectangleSolid tamanhoTabuleiroPixels tamanhoTabuleiroPixels)
        textoPrincipal = translate (-250) 100 $ scale 0.9 0.9 $ color white $ text "Parabens! 🏆"
        textoSecundario = translate (-350) 50 $ scale 0.3 0.3 $ color white $ text "Voce atingiu a peca de valor 2048!"
        textoMenu = translate (-275) (-150) $ scale 0.2 0.2 $ color white $ text "Pressione ENTER para voltar ao menu principal."
    in pictures [fundo, textoPrincipal, textoSecundario, textoMenu]

desenharJogo :: Jogo -> IO Picture
desenharJogo HUB = return desenharHUB
desenharJogo Dificuldade = return desenharTelaDificuldade
desenharJogo (Partida tabuleiro pontuacao) = return $ desenharPartida tabuleiro pontuacao
desenharJogo (GameOver _) = return desenharGameOver
desenharJogo (Vitoria _) = return desenharVitoria


verificaVitoria :: Tabuleiro -> Bool
verificaVitoria tabuleiro = any (elem 2048) tabuleiro

gameOver :: Tabuleiro -> Bool
gameOver tabuleiro =
    temCoordenadaLivre tabuleiro &&
    all (\mov -> mov tabuleiro == tabuleiro) [moverCima, moverBaixo, moverEsquerda, moverDireita]

resetarJogo :: Int -> IO Jogo
resetarJogo tamanho = do
    let novo = tabuleiroVazio tamanho
    (x, y) <- escolherPosicaoAleatoria novo
    let tabuleiroInicial = inserirPecaNova novo x y
    return (Partida tabuleiroInicial 0)

handleEvent :: Event -> Jogo -> IO Jogo
handleEvent (EventKey (SpecialKey KeyEnter) Down _ _) HUB = return Dificuldade

handleEvent (EventKey (Char dificuldade) Down _ _) Dificuldade = do
    case escolhaTabuleiro dificuldade of
        Just tamanho -> do
            let vazio = tabuleiroVazio tamanho
            (x, y) <- escolherPosicaoAleatoria vazio
            let tabuleiroInicial = inserirPecaNova vazio x y
            return $ Partida tabuleiroInicial 0
        Nothing -> return Dificuldade  

handleEvent (EventKey (SpecialKey KeyEnter) Down _ _) (GameOver _) = return HUB
handleEvent (EventKey (SpecialKey KeySpace) Down _ _) (GameOver tamanho) = resetarJogo tamanho

handleEvent (EventKey (SpecialKey KeyEnter) Down _ _) (Vitoria _) = return HUB

handleEvent (EventKey (SpecialKey KeyUp) Down _ _) (Partida tabuleiro pontuacao) = do
    let movido = moverCima tabuleiro
    if movido == tabuleiro
        then return $ Partida tabuleiro pontuacao
        else do
            (x, y) <- escolherPosicaoAleatoria movido
            let novoTabuleiro = inserirPecaNova movido x y
            if verificaVitoria novoTabuleiro
                then return $ Vitoria (length novoTabuleiro)
                else if gameOver novoTabuleiro
                    then return $ GameOver (length novoTabuleiro)
                    else return $ Partida novoTabuleiro (pontuacao + 1)

handleEvent (EventKey (SpecialKey KeyDown) Down _ _) (Partida tabuleiro pontuacao) = do
    let movido = moverBaixo tabuleiro
    if movido == tabuleiro
        then return $ Partida tabuleiro pontuacao
        else do
            (x, y) <- escolherPosicaoAleatoria movido
            let novoTabuleiro = inserirPecaNova movido x y
            if verificaVitoria novoTabuleiro
                then return $ Vitoria (length novoTabuleiro)
                else if gameOver novoTabuleiro
                    then return $ GameOver (length novoTabuleiro)
                    else return $ Partida novoTabuleiro (pontuacao + 1)

handleEvent (EventKey (SpecialKey KeyRight) Down _ _) (Partida tabuleiro pontuacao) = do
    let movido = moverDireita tabuleiro
    if movido == tabuleiro
        then return $ Partida tabuleiro pontuacao
        else do
            (x, y) <- escolherPosicaoAleatoria movido
            let novoTabuleiro = inserirPecaNova movido x y
            if verificaVitoria novoTabuleiro
                then return $ Vitoria (length novoTabuleiro)
                else if gameOver novoTabuleiro
                    then return $ GameOver (length novoTabuleiro)
                    else return $ Partida novoTabuleiro (pontuacao + 1)

handleEvent (EventKey (SpecialKey KeyLeft) Down _ _) (Partida tabuleiro pontuacao) = do
    let movido = moverEsquerda tabuleiro
    if movido == tabuleiro
        then return $ Partida tabuleiro pontuacao
        else do
            (x, y) <- escolherPosicaoAleatoria movido
            let novoTabuleiro = inserirPecaNova movido x y
            if verificaVitoria novoTabuleiro
                then return $ Vitoria (length novoTabuleiro)
                else if gameOver novoTabuleiro
                    then return $ GameOver (length novoTabuleiro)
                    else return $ Partida novoTabuleiro (pontuacao + 1)

handleEvent _ jogo = return jogo

janela :: Display
janela = InWindow "2048" (truncate tamanhoTabuleiroPixels * 2, truncate tamanhoTabuleiroPixels) (400, 600)

main :: IO ()
main = playIO janela (greyN 0.2) 60 HUB desenharJogo handleEvent (\_ tabuleiro -> return tabuleiro)