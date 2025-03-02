module Main where

import Graphics.Gloss.Interface.IO.Game (playIO)
import Graphics.Gloss.Interface.Pure.Game
import Tabuleiro (Tabuleiro, tamanhoJanela, tabuleiroVazio, alterarElemTabuleiro, desenharTelaDificuldade, tamanhoTabuleiro)
import Peca

data EstadoJogo 
    = Menu 
    | SelecionarDificuldade Int  -- Armazena a escolha do jogador (1 = fácil, 2 = médio, 3 = difícil)
    | Jogando Tabuleiro Int      -- O Int representa o tamanho do tabuleiro
    | GameOver Int               -- O Int representa o tamanho do tabuleiro
    deriving (Eq, Show)

-- Constantes
tamanhoCelula :: Float
tamanhoCelula = 100

corPeca :: Color
corPeca = makeColor 0.721568627 0.007843137 0.007843137 1

desenharTelaInicio :: Picture
desenharTelaInicio = pictures
    [ translate (-100) 50 (scale 0.5 0.5 (color white (text "2048")))
    , translate (-200) (-50) (scale 0.2 0.2 (color white (text "Pressione ENTER para jogar")))
    ]

-- Função para desenhar uma peça
desenharPeca :: Float -> Float -> Int -> Picture
desenharPeca posX posY valor = 
    let tamanho = tamanhoCelula * 0.8
    in pictures
        [ translate posX posY (color corPeca $ rectangleSolid tamanho tamanho)
        , translate (posX-15) (posY-20) (color white $ scale 0.4 0.4 $ text $ show valor)
        ]

-- Função para desenhar uma célula do tabuleiro
desenharCelula :: Int -> Int -> Int -> Int -> Picture
desenharCelula tamanho x y valor = 
    let posX = fromIntegral x * tamanhoCelula - fromIntegral tamanho * tamanhoCelula / 2 + tamanhoCelula / 2
        posY = fromIntegral (tamanho - 1 - y) * tamanhoCelula - fromIntegral tamanho * tamanhoCelula / 2 + tamanhoCelula / 2
        celula = translate posX posY $ color (greyN 0.5) $ rectangleSolid tamanhoCelula tamanhoCelula
        bordaCelula = translate posX posY $ color white $ rectangleWire tamanhoCelula tamanhoCelula
        peca = if valor >= 2 then desenharPeca posX posY valor else blank
    in pictures [celula, bordaCelula, peca]

-- Função para desenhar o tabuleiro
desenharTabuleiro :: Int -> Tabuleiro -> IO Picture
desenharTabuleiro tamanho tabuleiro = return $ pictures
    [ desenharCelula tamanho x y (tabuleiro !! y !! x) 
    | y <- [0..tamanho-1], x <- [0..tamanho-1]
    ]

desenharJogo :: EstadoJogo -> IO Picture
desenharJogo Menu = return desenharTelaInicio
desenharJogo (SelecionarDificuldade escolha) = return (desenharTelaDificuldade (Just escolha))
desenharJogo (Jogando tabuleiro tamanho) = desenharTabuleiro tamanho tabuleiro
desenharJogo (GameOver tamanho) = return (desenharGameOver tamanho)  -- Renderiza a tela de Game Over

desenharGameOver :: Int -> Picture
desenharGameOver tamanho =
    let tamanhoTabuleiroEmPixels = fromIntegral tamanho * tamanhoCelula
        posX = -tamanhoTabuleiroEmPixels / 2
        posY = -tamanhoTabuleiroEmPixels / 10 
        textoGameOver = scale 0.6 0.6 $ text "GAME OVER"
        corTexto = color red
        deslocamentos = [(dx, dy) | dx <- [-2,0,2], dy <- [-2,0,2]]  
        textoGrosso = pictures [translate dx dy (corTexto textoGameOver) | (dx, dy) <- deslocamentos]
    in pictures [
        translate posX posY textoGrosso,  
        translate (posX - 40) (posY - 40) $ color white $ scale 0.3 0.3 $ text "Pressione ENTER para reiniciar"
    ]

-- Manipular eventos
handleEvent :: Event -> EstadoJogo -> IO EstadoJogo
-- Transição do Menu para a tela de seleção de dificuldade
handleEvent (EventKey (SpecialKey KeyEnter) Down _ _) Menu = return (SelecionarDificuldade 1)

-- Captura da escolha do jogador na tela de seleção de dificuldade
handleEvent (EventKey (Char tecla) Down _ _) (SelecionarDificuldade _) = do
    case tamanhoTabuleiro tecla of
        Just tamanho -> do
            let novo = tabuleiroVazio tamanho
            (x, y) <- escolherPosicaoAleatoria $ coordenadasVazias tamanho novo
            let tabuleiroInicial = alterarElemTabuleiro novo x y 2
            return (Jogando tabuleiroInicial tamanho)
        Nothing -> return (SelecionarDificuldade 1)  -- Escolha inválida, permanece na tela de seleção

-- Reiniciar o jogo na tela de Game Over
handleEvent (EventKey (SpecialKey KeyEnter) Down _ _) (GameOver tamanho) = resetarJogo tamanho

-- Movimentação e verificação de Game Over
handleEvent (EventKey (SpecialKey KeyUp) Down _ _) (Jogando tabuleiro tamanho) = do
    let movido = moverCima tabuleiro
    if movido == tabuleiro
        then return (Jogando tabuleiro tamanho)
        else do
            (x, y) <- escolherPosicaoAleatoria $ coordenadasVazias tamanho movido
            let novoTabuleiro = alterarElemTabuleiro movido x y 2
            if gameOver novoTabuleiro
                then return (GameOver tamanho)
                else return (Jogando novoTabuleiro tamanho)

handleEvent (EventKey (SpecialKey KeyDown) Down _ _) (Jogando tabuleiro tamanho) = do
    let movido = moverBaixo tabuleiro
    if movido == tabuleiro
        then return (Jogando tabuleiro tamanho)
        else do
            (x, y) <- escolherPosicaoAleatoria $ coordenadasVazias tamanho movido
            let novoTabuleiro = alterarElemTabuleiro movido x y 2
            if gameOver novoTabuleiro
                then return (GameOver tamanho)
                else return (Jogando novoTabuleiro tamanho)

handleEvent (EventKey (SpecialKey KeyRight) Down _ _) (Jogando tabuleiro tamanho) = do
    let movido = moverDireita tabuleiro
    if movido == tabuleiro
        then return (Jogando tabuleiro tamanho)
        else do
            (x, y) <- escolherPosicaoAleatoria $ coordenadasVazias tamanho movido
            let novoTabuleiro = alterarElemTabuleiro movido x y 2
            if gameOver novoTabuleiro
                then return (GameOver tamanho)
                else return (Jogando novoTabuleiro tamanho)

handleEvent (EventKey (SpecialKey KeyLeft) Down _ _) (Jogando tabuleiro tamanho) = do
    let movido = moverEsquerda tabuleiro
    if movido == tabuleiro
        then return (Jogando tabuleiro tamanho)
        else do
            (x, y) <- escolherPosicaoAleatoria $ coordenadasVazias tamanho movido
            let novoTabuleiro = alterarElemTabuleiro movido x y 2
            if gameOver novoTabuleiro
                then return (GameOver tamanho)
                else return (Jogando novoTabuleiro tamanho)

-- Ignorar outros eventos
handleEvent _ estado = return estado

gameOver :: Tabuleiro -> Bool
gameOver tabuleiro =
    tabuleiroCheio tabuleiro &&
    all (\mov -> mov tabuleiro == tabuleiro) [moverCima, moverBaixo, moverEsquerda, moverDireita]

-- Reinicia o jogo
resetarJogo :: Int -> IO EstadoJogo
resetarJogo tamanho = do
    let novo = tabuleiroVazio tamanho
    (x, y) <- escolherPosicaoAleatoria $ coordenadasVazias tamanho novo
    let tabuleiroInicial = alterarElemTabuleiro novo x y 2
    return (Jogando tabuleiroInicial tamanho)

-- Configuração da janela
janela :: Int -> Display
janela tamanho = InWindow "2048" (truncate (tamanhoJanela tamanho), truncate (tamanhoJanela tamanho)) (0, 0)

-- Função principal
main :: IO ()
main = do
    let tamanhoInicial = 4  -- Tamanho padrão do tabuleiro
    playIO (janela tamanhoInicial) black 60 Menu desenharJogo handleEvent (\_ estado -> return estado)