--  Core game logic and rendering functions.
--  Contains the main step function (`gameStep`), drawing functions for game elements,
--  and event handling (keyboard input).


module GameLoop where

import Brillo
import Brillo.Interface.IO.Game
import Brillo.Data.Picture()
import Brillo.Data.Color()
import Linear()
import Wolf (Wolf(..), WolfStatus(..), jumpVelocity, gravity, defaultWolf)
import Obstacle (Obstacle(..), ObstacleType(..))
import System.Random (StdGen, randomR)
import GameState

cloudY :: Float
cloudY = 40  

-- constants only for collision. 

wolfW, wolfHRun, wolfHDuck :: Float
wolfW = 60      -- wolf's width while running or crouching
wolfHRun = 60   -- wolf's height when running/jumping
wolfHDuck = 40  -- wolf's height while crouching

collisionPadding :: ObstacleType -> Float
collisionPadding Cloud = 0.5
collisionPadding _     = 10.5

obstacleSize :: ObstacleType -> (Float, Float)
obstacleSize Rock = (50, 40)
obstacleSize Log = (65, 35)
obstacleSize Cloud = (80, 40)


--  collision test
isColliding :: Wolf -> Obstacle -> Bool
isColliding w o = 
    let 
        -- getting obstacle box
        (oW, oH)      = obstacleSize (otype o)
        -- wolf's height based on status
        wH            = if status w == Docking then wolfHDuck else wolfHRun
        -- center to center distances
        dx            = abs (wolfX w - obsX o)
        dy            = abs (wolfY w - obsY o)
        -- collision padding acc to obstacles
        padding       = collisionPadding (otype o)
        -- half sizes of each bos
        halfWidthSum  = (wolfW/2 + oW/2 - padding)
        halfHeightSum = (wH/2 + oH/2 - padding)
    -- check if overlaps in both axes
    in dx < halfWidthSum && dy < halfHeightSum

-- If the game is in playing phase then it moves the wolf by a certain distance
-- and return the updated wolf state
-- else if its in starting phase or game over phase then returns unchanged state.

gameStep :: Float -> GameState -> GameState
gameStep dt gs
  | phase gs /= Playing = gs
  | otherwise =
      let spd          = speed gs + 5 * dt
          w            = updateWolf dt spd (wolf gs)

          -- accumulate time for the 1‑point‑per‑second counter
          clk'         = scoreClock gs + dt
          (sc', clk'') =
              if clk' >= 1
                 then (score gs + 1, clk' - 1)   
                 else (score gs    , clk')      

          fc           = frameCounter gs + 1
          wolfX'       = wolfX w

          (obs', newLastX, newGen) =
            if wolfX' - lastObstacleX gs >= 600
               then let (newType, g') = randomObstacleType (rng gs)
                        newObsY = if newType == Cloud then cloudY else 0
                        newObs  = Obstacle (wolfX' + 800) newObsY newType
                    in (obstacles gs ++ [newObs], wolfX' + 600, g')
               else (obstacles gs, lastObstacleX gs, rng gs)

          hit          = any (isColliding w) obs'
        
        in 
            if hit 
                then gs {phase  = GameOver}
                else gs { wolf      = w
                , speed         = spd
                , score         = sc'
                , scoreClock    = clk''  
                , frameCounter  = fc
                , obstacles     = obs'
                , lastObstacleX = newLastX
                , rng           = newGen
            }

-- updates the wolf position
updateWolf :: Float -> Float -> Wolf -> Wolf
updateWolf dt spd w = case status w of
    Jumping -> 
        let vy = velocityY w + gravity
            y  = wolfY w + vy
            x  = wolfX w + (spd * 4) * dt 
        in if y <= 0
           then w { wolfY = 0, velocityY = 0, status = Running, wolfX = x }
           else w { wolfY = y, velocityY = vy, wolfX = x }

    Docking ->
        let x = wolfX w + spd * dt
        in w { wolfX = x }

    Running ->
        let x = wolfX w + spd * dt
        in w { wolfX = x }



-- genartes the intial obstacles
generateObstacles :: Wolf -> [Obstacle]
generateObstacles w = 
    [ Obstacle (wolfX w + 900) 0 Rock
    , Obstacle (wolfX w + 1600) 0 Log
    , Obstacle (wolfX w + 2300) cloudY Cloud
    ]


-- generated random obstacles
randomObstacleType :: StdGen -> (ObstacleType, StdGen)
randomObstacleType gen =
    let (n, newGen) = randomR (0 :: Int, 2) gen
        obstacleType = case n of
                  0 -> Rock
                  1 -> Log
                  _ -> Cloud
    in (obstacleType, newGen)



-- checks the currrent state of the game.
-- If its in StartScren phase then draws a Text
-- If its in playing state then renders wolf, obstacle and score.
-- If its in GameOver state then draws a text.
drawGame :: Picture -> Picture -> Picture -> Picture -> GameState -> Picture
drawGame wolfBMP rockBMP logBMP cloudBMP gs =
  let camX     = wolfX (wolf gs) + 250
      skyBlue  = makeColorI 135 206 235 255
      worldPic =
            translate (-camX) 0 $ pictures
              [ color skyBlue (rectangleSolid 3000 2000)
              , drawWolf       wolfBMP (wolf gs)
              , drawObstacles  rockBMP logBMP cloudBMP (obstacles gs)
              ]
  in case phase gs of
       StartScreen -> drawTextCentered "Press SPACE to Start" 0
       Playing     -> pictures [ worldPic           
                               , drawScore (score gs) ] 
       GameOver    -> drawTextCentered "Game Over! Press r to Restart" 0



-- Draws the wolf and during play, if gamer press
-- 's' then it docks.
drawWolf :: Picture -> Wolf -> Picture
drawWolf wolfBMP w =
  let crouching = status w == Docking
      -- normal scale = 1.4×1.4 ; crouch keeps width but halves height
      sx        = 1.4
      sy        = if crouching then 0.8 else 1.4
      -- drop it ~18 px so the feet stay on y = 0 when squashed
      yOffset   = if crouching then -18 else 0
  in translate (wolfX w)
               (wolfY w + yOffset) $
       scale sx sy wolfBMP



-- 	Maps over the obstacle list and draws each one, combining into a single picture.
drawObstacles :: Picture -> Picture -> Picture -> [Obstacle] -> Picture
drawObstacles rockBMP logBMP cloudBMP = pictures . map (drawObstacle rockBMP logBMP cloudBMP)


-- Draws a red obstacle based on its type and position.
drawObstacle :: Picture -> Picture -> Picture -> Obstacle -> Picture
drawObstacle rockBMP logBMP cloudBMP o =
    let scaledImage = case otype o of
                         Rock  -> scale 0.5 0.5 rockBMP
                         Log   -> scale 0.4 0.4 logBMP
                         Cloud -> scale 0.4 0.4 cloudBMP
        yOffset = case otype o of
                    Cloud -> 0  
                    _     -> -20 
    in translate (obsX o) (obsY o + yOffset) scaledImage


-- Displays score text in top-left.
drawScore :: Int -> Picture
drawScore s =
    let txt   = "Score: " ++ show s
        small = scale 0.3 0.3 . text
        bold  = pictures [ small txt
                         , translate 1 0 (small txt) ]
    in translate (-500) 350           
       $ color red bold



-- If the spacebar is pressed while on the start screen, switch to “Playing”.
--  If the game is in playing phase and user press space bar then the wolf
-- jumps. If the user press 's' and the game is in playing phase then the user
-- docks and if the user release the 's' char then the wolf comes back to running
-- phase.
handleEvent :: Event -> GameState -> GameState
handleEvent (EventKey (SpecialKey KeySpace) Down _ _) gs
    | phase gs == StartScreen = gs { phase = Playing }
    | phase gs == Playing && status (wolf gs) == Running =
        gs { wolf = (wolf gs) { velocityY = jumpVelocity, status = Jumping } }

handleEvent (EventKey (Char 's') Down _ _) gs
    | phase gs == Playing = gs { wolf = (wolf gs) { status = Docking } }

handleEvent (EventKey (Char 's') Up _ _) gs
    | phase gs == Playing = gs { wolf = (wolf gs) { status = Running } }

handleEvent (EventKey (Char 'r') Down _ _) gs
  | phase gs == GameOver =
      let fresh = GameState
            { wolf          = defaultWolf
            , obstacles     = generateObstacles defaultWolf
            , score         = 0
            , scoreClock    = 0
            , speed         = 200
            , phase         = Playing
            , frameCounter  = 0
            , rng           = rng gs
            , lastObstacleX = 0
            }
      in fresh

handleEvent _ gs = gs


--	Draws centered black text at given Y coordinate.
drawTextCentered :: String -> Float -> Picture
drawTextCentered txt y =
  let xOffset = - (fromIntegral (length txt) * 10)
  in translate xOffset y
       $ scale 0.5 0.5
       $ color black
       $ text txt