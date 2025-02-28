module Main where

import Graphics.Gloss.Interface.IO.Game (playIO)
import Graphics.Gloss.Interface.Pure.Game
import Tabuleiro
-- import Peca
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

mescla :: [Int] -> [Int] -- mescla elementos iguais
mescla [] = []
mescla [x] = [x]
mescla (x:y:xs)
    | x == y = (x + y) : mescla xs
    | otherwise = x : mescla (y : xs)

mesclarLinha :: [Int] -> [Int]
mesclarLinha linha = mesclada ++ replicate (length linha - length mesclada) 0
    where
        filtrada = filter (/= 0) linha
        mesclada = mescla filtrada

mesclarHorizontal :: EstadoJogo -> EstadoJogo
mesclarHorizontal = map mesclarLinha

mesclarVertical :: EstadoJogo -> EstadoJogo
mesclarVertical tabuleiro = transpor (mesclarHorizontal (transpor tabuleiro))

completarComZeros :: EstadoJogo -> EstadoJogo -- preenche os espaços com zeros. também garante que o tamanho do tabuleiro se mantenha
completarComZeros tabuleiro = map completarLinha tabuleiro
    where
        completarLinha linha = linha ++ replicate (tamanho - length linha) 0
        tamanho = maximum (map length tabuleiro)

moverEsquerda :: EstadoJogo -> EstadoJogo
moverEsquerda = mesclarHorizontal

moverDireita :: EstadoJogo -> EstadoJogo
moverDireita tabuleiro = map reverse (mesclarHorizontal (map reverse tabuleiro))

moverCima :: EstadoJogo -> EstadoJogo
moverCima = mesclarVertical

moverBaixo :: EstadoJogo -> EstadoJogo
moverBaixo tabuleiro = map reverse (transpor (mesclarHorizontal (transpor (map reverse tabuleiro))))

handleEvent :: Event -> EstadoJogo -> IO EstadoJogo -- esperando moverPeca
handleEvent (EventKey (SpecialKey KeyUp)     Down _ _) eJogo = do -- tecla setinha pra cima
    (x, y) <- coordenadasAleatorias 0 (tamanhoGrade-1)
    let novoEJogo = alterarElemTabuleiro (moverCima eJogo) x y 2
    return novoEJogo
handleEvent (EventKey (SpecialKey KeyDown)   Down _ _) eJogo = do -- tecla setinha pra cima
    (x, y) <- coordenadasAleatorias 0 (tamanhoGrade-1)
    let novoEJogo = alterarElemTabuleiro (moverBaixo eJogo) x y 2
    return novoEJogo
handleEvent (EventKey (SpecialKey KeyRight)  Down _ _) eJogo = do -- tecla setinha pra direita
    (x, y) <- coordenadasAleatorias 0 (tamanhoGrade-1)
    let novoEJogo = alterarElemTabuleiro (moverBaixo eJogo) x y 2
    return novoEJogo
handleEvent (EventKey (SpecialKey KeyLeft)   Down _ _) eJogo = do -- tecla setinha pra esquerda
    (x, y) <- coordenadasAleatorias 0 (tamanhoGrade-1)
    let novoEJogo = alterarElemTabuleiro (moverBaixo eJogo) x y 2
    return novoEJogo
handleEvent _ eJogo = return eJogo -- ignora resto do teclado e não faz nada

janela :: Display
janela = InWindow "Tabuleiro" (truncate larguraTab, truncate alturaTab) (0, 0)

main :: IO ()
main = do
    (x, y) <- coordenadasAleatorias 0 (tamanhoGrade-1)

    let tabuleiroInicial = alterarElemTabuleiro (tabuleiroVazio tamanhoGrade) x y 2
    
    playIO janela white 60 tabuleiroInicial desenharJogo handleEvent (\_ estado -> return estado)
