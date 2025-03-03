module Main where
import qualified Data.Map as Map  -- (importei p usar na pontuação)
import Graphics.Gloss.Interface.IO.Game (playIO)
import Graphics.Gloss.Interface.Pure.Game
import Tabuleiro
import Peca


data Jogo = HUB | Dificuldade Int | Partida Tabuleiro Int Int | GameOver Int

desenharHUB :: Picture
desenharHUB = pictures
    [ translate (-100) 50 (scale 0.5 0.5 (color white (text "2048")))
    , translate (-200) (-50) (scale 0.2 0.2 (color white (text "Pressione ENTER para jogar")))
    ]

desenharTelaDificuldade :: Maybe Int -> Picture
desenharTelaDificuldade escolha = pictures [
      translate (-200) 100 (scale 0.5 0.5 (color white (text "Escolha a Dificuldade")))
    , translate (-200) 50 (scale 0.3 0.3 (color (corEscolha 4) (text "1 - Facil (4x4)")))
    , translate (-200) 0 (scale 0.3 0.3 (color (corEscolha 5) (text "2 - Medio (5x5)")))
    , translate (-200) (-50) (scale 0.3 0.3 (color (corEscolha 6) (text "3 - Dificil (6x6)")))
    , translate (-200) (-150) (scale 0.2 0.2 (color white (text "Pressione 1, 2 ou 3 para escolher")))
    ]
    where
        corEscolha tamanho = if escolha == Just tamanho then green else white

desenharPeca :: Float -> Float -> Peca -> Picture
desenharPeca posX posY valor =
    let tamanho = tamanhoCelula * 0.8
        in pictures [translate posX posY (color red $ rectangleSolid tamanho tamanho),
                     translate (posX-15) (posY-20) (color white $ scale 0.4 0.4 $ text $ show valor)]

desenharCelula :: Int -> Int -> Int -> Peca -> Picture
desenharCelula tamanhoTab x y valor = pictures [celula, bordaCelula, peca]
    where
        posX = fromIntegral x * tamanhoCelula - tamanhoJanela tamanhoTab / 2 + tamanhoCelula / 2
        posY = fromIntegral (tamanhoTab - 1 - y) * tamanhoCelula - tamanhoJanela tamanhoTab / 2 + tamanhoCelula / 2

        celula = translate posX posY $ color (greyN 0.5) $ rectangleSolid tamanhoCelula tamanhoCelula
        bordaCelula = translate posX posY $ color white $ rectangleWire tamanhoCelula tamanhoCelula

        peca = if valor >= 2 then desenharPeca posX posY valor else blank

desenharTabuleiro :: Tabuleiro -> Picture
desenharTabuleiro tabuleiro = pictures [desenharCelula (length tabuleiro ) x y (tabuleiro !! y !! x) | y <- [0..length tabuleiro - 1], x <- [0..length tabuleiro - 1]]

desenharPontuacao :: Int -> Int -> Picture
desenharPontuacao tamanhoTab pontuacao =
    translate posX posY $ scale 0.5 0.5 $ color white $ text ("Score: " ++ show pontuacao)
    where
        posX = -(tamanhoJanela tamanhoTab / 2 + 20)
        posY = tamanhoJanela tamanhoTab / 2 - 50

desenharPartida :: Tabuleiro -> Int -> Int -> Picture
desenharPartida tabuleiro tamanho pontuacao = pictures [desenharTabuleiro tabuleiro, desenharPontuacao tamanho pontuacao] -- !!! modificar desenharTabuleiro !!!

desenharGameOver :: Int -> Picture
desenharGameOver tamanho =
    let tamanhoTabuleiroEmPixels = fromIntegral tamanho * tamanhoCelula
        posX = -tamanhoTabuleiroEmPixels
        posY = tamanhoTabuleiroEmPixels / 2
        textoGameOver = scale 0.6 0.6 $ text "GAME OVER"
        corTexto = color red
        deslocamentos = [(dx, dy) | dx <- [-2,0,2], dy <- [-2,0,2]]
        textoGrosso = pictures [translate dx dy (corTexto textoGameOver) | (dx, dy) <- deslocamentos]
    in pictures [
        translate posX posY textoGrosso,
        translate (posX - 50) (posY - 100) $ color white $ scale 0.3 0.3 $ text "Pressione ENTER para reiniciar"
    ]

desenharJogo :: Jogo -> IO Picture
desenharJogo HUB = return desenharHUB
desenharJogo (Dificuldade dificuldade) = return $ desenharTelaDificuldade (Just dificuldade)
desenharJogo (Partida tabuleiro tamanho pontuacao) = return $ desenharPartida tabuleiro tamanho pontuacao
desenharJogo (GameOver tamanho) = return $ desenharGameOver tamanho

gameOver :: Tabuleiro -> Bool
gameOver tabuleiro =
    temCoordenadaLivre tabuleiro &&
    all (\mov -> mov tabuleiro == tabuleiro) [moverCima, moverBaixo, moverEsquerda, moverDireita]

resetarJogo :: Int -> IO Jogo
resetarJogo tamanho = do
    let novo = tabuleiroVazio tamanho
    (x, y) <- escolherPosicaoAleatoria novo
    let tabuleiroInicial = alterarElemTabuleiro novo x y 2
    return (Partida tabuleiroInicial tamanho 0)

handleEvent :: Event -> Jogo -> IO Jogo
handleEvent (EventKey (SpecialKey KeyEnter) Down _ _) HUB = return (Dificuldade 0)

handleEvent (EventKey (Char dificuldade) Down _ _) (Dificuldade _) = do
    case escolhaTabuleiro dificuldade of
        Just tamanho -> do                     -- incia o jogo com um tabuleiro novo
            let vazio = tabuleiroVazio tamanho
            (x, y) <- escolherPosicaoAleatoria vazio
            let tabuleiroInicial = inserirPecaNova vazio x y
            return $ Partida tabuleiroInicial tamanho 0 --pontuacao comça com 0

        Nothing -> return (Dificuldade 0)      -- se mantém na tela de escolha de Dificuldade

handleEvent (EventKey (SpecialKey KeyEnter) Down _ _) (GameOver tamanho) = resetarJogo tamanho

handleEvent (EventKey (SpecialKey KeyUp) Down _ _) (Partida tabuleiro tamanho pontuacao) = do
    let (movido, scoreIncrement) = moverComPontuacao tabuleiro moverCima 
    if movido == tabuleiro -- movimento não afeta as peças, retorna o tabueiro sem atualizar
        then return $ Partida tabuleiro tamanho pontuacao
        else do
            (x, y) <- escolherPosicaoAleatoria movido
            let novoTabuleiro = inserirPecaNova movido x y
            if gameOver novoTabuleiro
                then return $ GameOver tamanho
                else return $ Partida novoTabuleiro tamanho (pontuacao + scoreIncrement)         -- !!! consertar pontuação !!!

handleEvent (EventKey (SpecialKey KeyDown) Down _ _) (Partida tabuleiro tamanho pontuacao) = do
    let (movido, scoreIncrement) = moverComPontuacao tabuleiro moverBaixo 
    if movido == tabuleiro
        then return $ Partida tabuleiro tamanho pontuacao
        else do
            (x, y) <- escolherPosicaoAleatoria movido
            let novoTabuleiro = inserirPecaNova movido x y
            if gameOver novoTabuleiro
                then return $ GameOver tamanho
                else return $ Partida novoTabuleiro tamanho (pontuacao + scoreIncrement)         -- !!! consertar pontuação !!!

handleEvent (EventKey (SpecialKey KeyRight) Down _ _) (Partida tabuleiro tamanho pontuacao) = do
    let (movido, scoreIncrement) = moverComPontuacao tabuleiro moverDireita 
    if movido == tabuleiro
        then return $ Partida tabuleiro tamanho pontuacao
        else do
            (x, y) <- escolherPosicaoAleatoria movido
            let novoTabuleiro = inserirPecaNova movido x y
            if gameOver novoTabuleiro
                then return $ GameOver tamanho
                else return $ Partida novoTabuleiro tamanho (pontuacao + scoreIncrement)        -- !!! consertar pontuação !!!

handleEvent (EventKey (SpecialKey KeyLeft) Down _ _) (Partida tabuleiro tamanho pontuacao) = do
    let (movido, scoreIncrement) = moverComPontuacao tabuleiro moverEsquerda 
    if movido == tabuleiro
        then return $ Partida tabuleiro tamanho pontuacao
        else do
            (x, y) <- escolherPosicaoAleatoria movido
            let novoTabuleiro = inserirPecaNova movido x y
            if gameOver novoTabuleiro
                then return $ GameOver tamanho
                else return $ Partida novoTabuleiro tamanho (pontuacao + scoreIncrement)         -- !!! consertar pontuação !!!

handleEvent _ jogo = return jogo -- Ignora o restante do teclado e não faz nada
-------------------------------------------------------------------------------------------------------------------
calculaScore :: Tabuleiro -> Tabuleiro -> Int
calculaScore antes depois =
  let
    -- Constrói um mapa (dicionário) que conta as ocorrências de cada peça (exceto 0)
    buildMap :: Tabuleiro -> Map.Map Int Int
    buildMap tab = foldl (\acc linha -> 
                          foldl (\acc' x -> if x == 0 
                                            then acc' 
                                            else Map.insertWith (+) x 1 acc' 
                                 ) acc linha
                         ) Map.empty tab

    mapaAntes  = buildMap antes
    mapaDepois = buildMap depois

    -- Para cada valor presente no mapaDepois, calcula a diferença na contagem
    score = Map.foldrWithKey (\valor cnt acc -> 
              let cntAntes = Map.findWithDefault 0 valor mapaAntes
                  diff = cnt - cntAntes
              in if diff > 0 then acc + diff * valor else acc
            ) 0 mapaDepois
  in score


-- Função para mover e calcular a pontuação resultante das mesclas
moverComPontuacao :: Tabuleiro -> (Tabuleiro -> Tabuleiro) -> (Tabuleiro, Int)
moverComPontuacao tabuleiro movimento = 
    let novoTabuleiro = movimento tabuleiro
        scoreIncrement = calculaScore tabuleiro novoTabuleiro
    in (novoTabuleiro, scoreIncrement)

-------------------------------------------------------------------------------------------------------------------

janela :: Int -> Display
janela tamanho = InWindow "2048" (truncate $ tamanhoJanela tamanho, truncate $ tamanhoJanela tamanho) (0, 0)

main :: IO ()
main = playIO (janela 5) black 60 HUB desenharJogo handleEvent (\_ tabuleiro -> return tabuleiro)
