module Main where

import Graphics.Gloss
    ( white, display, Display(InWindow), Picture(Circle) )

main :: IO ()
main = display window white drawing
  where
    window  = InWindow "Minha Janela" (700, 400) (100, 100)
    drawing = Circle 75
