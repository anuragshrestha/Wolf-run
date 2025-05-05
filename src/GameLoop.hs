--  Core game logic and rendering functions.
--  Contains the main step function (`gameStep`), drawing functions for game elements,
--  and event handling (keyboard input).


module GameLoop where

import Brillo
import Brillo.Interface.IO.Game
import Brillo.Data.Picture()
import Brillo.Data.Color()
import Linear()
import Wolf (Wolf(..))
import Obstacle (Obstacle(..), ObstacleType(..))


import GameState

gameStep :: Float -> GameState -> GameState
gameStep _ gs = gs

drawGame :: Picture -> GameState -> Picture
drawGame wolfBMP gs =
    case phase gs of
        StartScreen ->
            drawTextCentered "Press SPACE to Start" 0
        Playing ->
            pictures
                [ drawWolf wolfBMP (wolf gs)
                , drawObstacles (obstacles gs)
                , drawScore (score gs)
                ]
        GameOver ->
            drawTextCentered "Game Over! Press R to Restart" 0

drawWolf :: Picture -> Wolf -> Picture
drawWolf wolfBMP w =
    translate (wolfX w) (wolfY w) $
    scale 0.25 0.25 
    wolfBMP

drawObstacles :: [Obstacle] -> Picture
drawObstacles = pictures . map drawObstacle

drawObstacle :: Obstacle -> Picture
drawObstacle o =
    translate (obsX o) (obsY o) $
      color red $
        case otype o of
            Log   -> rectangleSolid 60 20
            Rock  -> circleSolid 15
            Cloud -> scale 1.5 1.0 $ circleSolid 10

drawScore :: Int -> Picture
drawScore s = translate (-500) 350 $ scale 0.3 0.3 $
    text ("Score: " ++ show s)

handleEvent :: Event -> GameState -> GameState
handleEvent (EventKey (SpecialKey KeySpace) Down _ _) gs
    | phase gs == StartScreen = gs { phase = Playing }
handleEvent _ gs = gs

drawTextCentered :: String -> Float -> Picture
drawTextCentered txt y =
    translate (-200) y $
        scale 0.5 0.5 $ 
        color black $
            text txt