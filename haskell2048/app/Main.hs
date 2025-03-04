module Main where

import Graphics.Gloss.Interface.IO.Game (playIO)
import Graphics.Gloss.Interface.Pure.Game
import Tabuleiro
import Peca


data Jogo = TelaInicio | TelaDificuldade | Partida Tabuleiro Int Bool | GameOver Int | Vitoria Tabuleiro Int

desenharTelaInicio :: Picture
desenharTelaInicio = pictures
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
desenharPeca tamanhoJogo posX posY valor = pictures [peca, valorPeca]
    where
        tamanho = tamanhoCelula tamanhoJogo * 0.8

        peca      = translate posX posY (color (corPeca valor) $ rectangleSolid tamanho tamanho)
        valorPeca = translate (posX-15) (posY-20) (color black $ scale 0.4 0.4 $ text $ show valor)

desenharCelula :: Int -> Int -> Int -> Peca -> Picture
desenharCelula tamanhoJogo x y valor = pictures [celula, bordaCelula, peca]
    where
        posX = fromIntegral x * tamanhoCelula tamanhoJogo - tamanhoTabuleiroPixels / 2 + tamanhoCelula tamanhoJogo / 2
        posY = fromIntegral (tamanhoJogo - 1 - y) * tamanhoCelula tamanhoJogo - tamanhoTabuleiroPixels / 2 + tamanhoCelula tamanhoJogo / 2

        celula      = translate posX posY $ color (greyN 0.3) $ rectangleSolid (tamanhoCelula tamanhoJogo) (tamanhoCelula tamanhoJogo)
        bordaCelula = translate posX posY $ color white $ rectangleWire (tamanhoCelula tamanhoJogo) (tamanhoCelula tamanhoJogo)

        peca = if valor >= 2 then desenharPeca tamanhoJogo posX posY valor else blank

desenharTabuleiro :: Tabuleiro -> Picture
desenharTabuleiro tabuleiro = pictures celulasTabuleiro
    where 
        celulasTabuleiro = [desenharCelula (length tabuleiro) x y (tabuleiro !! y !! x) | 
                            y <- [0..length tabuleiro - 1],
                            x <- [0..length tabuleiro - 1]]

desenharPontuacao :: Int -> Picture
desenharPontuacao pontuacao = textoPontuacao
    where
        posX = 50 + tamanhoTabuleiroPixels / 2
        posY = tamanhoTabuleiroPixels / 2 - 50
        
        textoPontuacao = translate posX posY $ scale 0.4 0.4 $ color white $ text ("Score: " ++ show pontuacao)

desenharPartida :: Tabuleiro -> Int -> Picture
desenharPartida tabuleiro pontuacao = pictures [desenharTabuleiro tabuleiro, desenharPontuacao pontuacao]

desenharGameOver :: Picture
desenharGameOver = pictures [textoGameOver, textoReiniciar, textoDificuldade]
    where
        texto            = scale 0.6 0.6 $ text "GAME OVER"
        corTexto         = color red
        deslocamentos    = [(dx, dy) | dx <- [-2,0,2], dy <- [-2,0,2]]
        textoGrosso      = pictures [translate dx dy (corTexto texto) | (dx, dy) <- deslocamentos]
        textoGameOver    = translate (-225) 100 textoGrosso
        textoReiniciar   = translate (-275) 0 $ color white $ scale 0.3 0.3 $ text "Pressione 1 para reiniciar"
        textoDificuldade = translate (-275) (-50) $ color white $ scale 0.3 0.3 $ text "Pressione 2 para mudar a dificuldade"

desenharVitoria :: Picture
desenharVitoria = pictures [fundo, textoPrincipal, textoSecundario, textoContinuar, textoRecomecar, textoParar]
    where
        fundo           = color (makeColorI 255 215 0 1) (rectangleSolid tamanhoTabuleiroPixels tamanhoTabuleiroPixels)
        textoPrincipal  = translate (-200) 100    $ scale 0.9 0.9 $ color white $ text "VITORIA! 🏆"
        textoSecundario = translate (-300) 50     $ scale 0.3 0.3 $ color white $ text "Voce conquistou a peca de valor 2048!"
        textoContinuar  = translate (-300) (-150) $ scale 0.2 0.2 $ color white $ text "Pressione 1 se quiser continuar jogando."
        textoRecomecar  = translate (-300) (-200) $ scale 0.2 0.2 $ color white $ text "Pressione 2 se quiser recomecar a partida."
        textoParar      = translate (-300) (-250) $ scale 0.2 0.2 $ color white $ text "Pressione 3 se quiser recomecar a partida."

desenharJogo :: Jogo -> IO Picture
desenharJogo TelaInicio = return desenharTelaInicio
desenharJogo TelaDificuldade = return desenharTelaDificuldade
desenharJogo (Partida tabuleiro pontuacao _) = return $ desenharPartida tabuleiro pontuacao
desenharJogo (GameOver _) = return desenharGameOver
desenharJogo (Vitoria _ _) = return desenharVitoria

verificaVitoria :: Tabuleiro -> Bool
verificaVitoria = any (elem 2048)

gameOver :: Tabuleiro -> Bool
gameOver tabuleiro =
    temCoordenadaLivre tabuleiro &&
    all (\mov -> mov tabuleiro == tabuleiro) [moverCima, moverBaixo, moverEsquerda, moverDireita]

resetarJogo :: Int -> IO Jogo
resetarJogo tamanho = do
    let vazio = tabuleiroVazio tamanho
    (x, y) <- escolherPosicaoAleatoria vazio
    let novo = inserirPecaNova vazio x y
    return (Partida novo 0 False)

handleEvent :: Event -> Jogo -> IO Jogo
handleEvent (EventKey (SpecialKey KeyEnter) Down _ _) TelaInicio = return TelaDificuldade

handleEvent (EventKey (Char dificuldade) Down _ _) TelaDificuldade =
    case escolhaTabuleiro dificuldade of
        Just tamanho -> do
            let vazio = tabuleiroVazio tamanho
            (x, y) <- escolherPosicaoAleatoria vazio
            let novoTabuleiro = inserirPecaNova vazio x y
            return $ Partida novoTabuleiro 0 False
        Nothing -> return TelaDificuldade

handleEvent (EventKey (Char opcao) Down _ _) (GameOver tamanho) =
    case opcao of
        '1' -> resetarJogo tamanho
        '2' -> return TelaDificuldade
        _ -> return $ GameOver tamanho -- não faz nada

handleEvent (EventKey (Char opcao) Down _ _) (Vitoria tabuleiro pontuacao) =
    case opcao of
        '1' -> return $ Partida tabuleiro pontuacao True -- voltar a jogar de onde parou
        '2' -> resetarJogo $ length tabuleiro -- reinicia a partida
        '3' -> return TelaDificuldade -- volta para escolher a dificuldade
        _ -> return $ Vitoria tabuleiro pontuacao -- não faz nada

handleEvent (EventKey (SpecialKey KeyUp) Down _ _) (Partida tabuleiro pontuacao statusVitoria) = do
    let movido = moverCima tabuleiro
    if movido == tabuleiro
        then return $ Partida tabuleiro pontuacao statusVitoria
        else do
            (x, y) <- escolherPosicaoAleatoria movido
            let novoTabuleiro = inserirPecaNova movido x y
            if statusVitoria -- se o jogador JÁ TIVER GANHADO, não precisa verificar a vitória
                then if gameOver novoTabuleiro
                        then return $ GameOver $ length novoTabuleiro
                        else return $ Partida novoTabuleiro pontuacao statusVitoria
                else -- mas se ainda não tiver ganho a partida
                    if verificaVitoria novoTabuleiro -- verifica a vitoria
                        then return $ Vitoria novoTabuleiro pontuacao 
                        else if gameOver novoTabuleiro
                                then return $ GameOver (length novoTabuleiro)
                                else return $ Partida novoTabuleiro pontuacao statusVitoria

handleEvent (EventKey (SpecialKey KeyDown) Down _ _) (Partida tabuleiro pontuacao statusVitoria) = do
    let movido = moverBaixo tabuleiro
    if movido == tabuleiro
        then return $ Partida tabuleiro pontuacao statusVitoria
        else do
            (x, y) <- escolherPosicaoAleatoria movido
            let novoTabuleiro = inserirPecaNova movido x y
            if statusVitoria -- se o jogador JÁ TIVER GANHADO, não precisa verificar a vitória
                then if gameOver novoTabuleiro
                        then return $ GameOver $ length novoTabuleiro
                        else return $ Partida novoTabuleiro pontuacao statusVitoria
                else -- mas se ainda não tiver ganho a partida
                    if verificaVitoria novoTabuleiro -- verifica a vitoria
                        then return $ Vitoria novoTabuleiro pontuacao 
                        else if gameOver novoTabuleiro
                                then return $ GameOver $ length novoTabuleiro
                                else return $ Partida novoTabuleiro pontuacao statusVitoria

handleEvent (EventKey (SpecialKey KeyRight) Down _ _) (Partida tabuleiro pontuacao statusVitoria) = do
    let movido = moverDireita tabuleiro
    if movido == tabuleiro
        then return $ Partida tabuleiro pontuacao statusVitoria
        else do
            (x, y) <- escolherPosicaoAleatoria movido
            let novoTabuleiro = inserirPecaNova movido x y
            if statusVitoria -- se o jogador JÁ TIVER GANHADO, não precisa verificar a vitória
                then if gameOver novoTabuleiro
                        then return $ GameOver $ length novoTabuleiro
                        else return $ Partida novoTabuleiro pontuacao statusVitoria
                else -- mas se ainda não tiver ganho a partida
                    if verificaVitoria novoTabuleiro -- verifica a vitoria
                        then return $ Vitoria novoTabuleiro pontuacao 
                        else if gameOver novoTabuleiro
                                then return $ GameOver $ length novoTabuleiro
                                else return $ Partida novoTabuleiro pontuacao statusVitoria

handleEvent (EventKey (SpecialKey KeyLeft) Down _ _) (Partida tabuleiro pontuacao statusVitoria) = do
    let movido = moverEsquerda tabuleiro
    if movido == tabuleiro
        then return $ Partida tabuleiro pontuacao statusVitoria
        else do
            (x, y) <- escolherPosicaoAleatoria movido
            let novoTabuleiro = inserirPecaNova movido x y
            if statusVitoria -- se o jogador JÁ TIVER GANHADO, não precisa verificar a vitória
                then if gameOver novoTabuleiro
                        then return $ GameOver $ length novoTabuleiro 
                        else return $ Partida novoTabuleiro pontuacao statusVitoria
                else -- mas se ainda não tiver ganho a partida
                    if verificaVitoria novoTabuleiro -- verifica a vitoria
                        then return $ Vitoria novoTabuleiro pontuacao
                        else if gameOver novoTabuleiro
                                then return $ GameOver $ length novoTabuleiro
                                else return $ Partida novoTabuleiro pontuacao statusVitoria
handleEvent _ jogo = return jogo

janela :: Display
janela = InWindow "2048" (truncate tamanhoTabuleiroPixels * 2, truncate tamanhoTabuleiroPixels * 2) (400, 600)

main :: IO ()
main = playIO janela (greyN 0.2) 60 TelaInicio desenharJogo handleEvent (\_ tabuleiro -> return tabuleiro)
