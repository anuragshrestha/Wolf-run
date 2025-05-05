-- | Entry point for the Wolf Run game.
--   Initializes the window, sets up the game loop, and starts rendering and input handling.

module Main where

import Wolf (defaultWolf)
import Brillo
-- import Brillo.Interface.Pure.Game (play)
-- import Brillo.Data.Bitmap (loadBMP)
import GameLoop
import GameState

main :: IO ()
main = do
    wolfBMP <- loadBMP "assets/wolf.bmp"

    let initialState = GameState
          { wolf = defaultWolf
          , obstacles = []
          , score = 0
          , speed = 5
          , phase = StartScreen
          }

    play
      (InWindow "Wolf Run" (1200, 800) (100, 100))  
      white
      60
      initialState
      (drawGame wolfBMP)
      handleEvent
      gameStep
    
  