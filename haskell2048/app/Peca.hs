module Peca where

import Graphics.Gloss

import System.Random (StdGen, Random(randomR))


geraPeca :: StdGen -> Int -> Int -> (Int, Int, Int) -- função que gera as posições aleatórias, a peça sai com o valor default de 2
geraPeca gen i f =
    let (x, gen')  = randomR (i, f) gen
        (y, _) = randomR (i, f) gen'
        in (x, y, 2)

-- desenhaPeca :: [(Int, Int, Int)] -> Float -> Float -> Float -> Picture -- retorna uma Picture com formato quadrado que representa a Peça
-- desenhaPeca estado tamCel larg alt =
--     pictures
--         [
--             translate (fromIntegral x * tamCel - larg / 2 + tamCel / 2)
--                       (fromIntegral y * tamCel - alt / 2 + tamCel / 2)
--                       (color red (rectangleSolid (tamCel * 0.8) (tamCel * 0.8)))
--                       | (x, y, _) <- estado
--         ]

desenhaPecas :: [(Int, Int, Int)] -> Float -> Float -> Float -> Picture
desenhaPecas estado tamCel larg alt = pictures (map desenhaPeca pecas)
    where
        pecas = [(x, y, n) | (x, y, n) <- estado]

        desenhaPeca :: (Int, Int, Int) -> Picture
        desenhaPeca (x, y, n) = pictures [pecaQuadrada, valor]
            where
            pecaQuadrada = translate posX posY (color red (rectangleSolid (tamCel * 0.8) (tamCel * 0.8)))
            valor = translate posX posY (color white (scale 0.2 0.2 (text (show n))))
            
            posX = fromIntegral x * tamCel - larg / 2 + tamCel / 2
            posY = fromIntegral y * tamCel - alt / 2 + tamCel / 2
