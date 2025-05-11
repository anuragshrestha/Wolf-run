-- | Entry point for the Wolf Run game.
--   Initializes the window, sets up the game loop, and starts rendering and input handling.

module Main where

import Wolf (defaultWolf)
import Brillo
-- import Brillo.Interface.Pure.Game (play)
-- import Brillo.Data.Bitmap (loadBMP)
import GameLoop
import GameState
import System.Random (getStdGen)

-- Declares the main function as an IO action
-- Loads the wolf image
-- Constructs the initial game state:
--	Wolf at default position,
--	No obstacles,
--	Score is 0,
--	Speed is 5 (units per frame),
--	Phase is the start screen.




main :: IO ()
main = do
    wolfBMP <- loadBMP "assets/wolf.bmp"
    rockBMP <- loadBMP "assets/rock.bmp"
    logBMP  <- loadBMP "assets/log.bmp"
    cloudBMP <- loadBMP "assets/cloud.bmp"
    gen <- getStdGen  -- generate initial RNG

    let initialState = GameState
            { wolf = defaultWolf
            , obstacles = generateObstacles defaultWolf
            , score = 0
            , scoreClock    = 0  
            , speed = 70
            , phase = StartScreen
            , frameCounter = 0
            , rng = gen
            , lastObstacleX = 1800 
            }
    let skyBlue = makeColorI 135 206 235 255

    play
      (InWindow "Wolf Run" (1400, 1000) (80, 80))  
      skyBlue
      60
      initialState
      (drawGame wolfBMP rockBMP logBMP cloudBMP)
      handleEvent
      gameStep


  