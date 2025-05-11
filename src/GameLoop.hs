--  Core game logic and rendering functions.
--  Contains the main step function (`gameStep`), drawing functions for game elements,
--  and event handling (keyboard input).


module GameLoop where
import Brillo
import Brillo.Interface.IO.Game
import Brillo.Data.Picture()
import Brillo.Data.Color()
import Linear()
import Wolf (Wolf(..), WolfStatus(..), jumpVelocity, gravity)
import Obstacle (Obstacle(..), ObstacleType(..))
import System.Random (StdGen, randomR)
import GameState




-- If the game is in playing phase then it moves the wolf by a certain distance
-- and return the updated wolf state
-- else if its in starting phase or game over phase then returns unchanged state.

gameStep :: Float -> GameState -> GameState
gameStep dt gs
  | phase gs == Playing =
      let spd = speed gs + 3 * dt  
          w   = updateWolf dt spd (wolf gs)  
          fc  = frameCounter gs + 1
          wolfX' = wolfX w

          (obs', newLastX, newGen) =
            if wolfX' - lastObstacleX gs >= 300 then
              let (newType, g') = randomObstacleType (rng gs)
                  newObsY = case newType of
                              Cloud -> 300
                              _     -> 0
                  newObs = Obstacle (wolfX' + 250) newObsY newType
              in (obstacles gs ++ [newObs], wolfX' + 250, g')
            else (obstacles gs, lastObstacleX gs, rng gs)

      in gs { wolf = w
            , speed = spd
            , frameCounter = fc
            , obstacles = obs'
            , lastObstacleX = newLastX
            , rng = newGen
            }

  | otherwise = gs


-- updates the wolf position
updateWolf :: Float -> Float -> Wolf -> Wolf
updateWolf dt spd w = case status w of
    Jumping -> 
        let vy = velocityY w + gravity
            y  = wolfY w + vy
            x  = wolfX w + (spd * 4.8) * dt 
        in if y <= 0
           then w { wolfY = 0, velocityY = 0, status = Running, wolfX = x }
           else w { wolfY = y, velocityY = vy, wolfX = x }

    Docking ->
        let x = wolfX w + spd * dt
        in w { wolfX = x }

    Running ->
        let x = wolfX w + spd * dt
        in w { wolfX = x }



-- genartes the obstacles
generateObstacles :: Wolf -> [Obstacle]
generateObstacles w = 
    [ Obstacle (wolfX w + 600) 0 Rock
    , Obstacle (wolfX w + 1000) 0 Log
    , Obstacle (wolfX w + 1400) 40 Cloud
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
    let camX = wolfX (wolf gs) - 200
        skyBlue = makeColorI 135 206 235 255  -- RGB for sky blue
        background = color skyBlue $ rectangleSolid 3000 2000 
    in case phase gs of
        StartScreen ->
            drawTextCentered "Press SPACE to Start" 0
        Playing ->
            translate (-camX) 0 $ pictures
                [ background
                , drawWolf wolfBMP (wolf gs)
                , drawObstacles rockBMP logBMP cloudBMP (obstacles gs)
                , drawScore (score gs)
                ]
        GameOver ->
            drawTextCentered "Game Over! Press R to Restart" 0



drawWolf :: Picture -> Wolf -> Picture
drawWolf wolfBMP w =
    translate (wolfX w) (wolfY w) $
    scale 0.15 0.15 
    wolfBMP


-- 	Maps over the obstacle list and draws each one, combining into a single picture.
drawObstacles :: Picture -> Picture -> Picture -> [Obstacle] -> Picture
drawObstacles rockBMP logBMP cloudBMP = pictures . map (drawObstacle rockBMP logBMP cloudBMP)


-- Draws a red obstacle based on its type and position.
drawObstacle :: Picture -> Picture -> Picture -> Obstacle -> Picture
drawObstacle rockBMP logBMP cloudBMP o =
    let scaledImage = case otype o of
                         Rock  -> scale 0.3 0.3 rockBMP
                         Log   -> scale 0.3 0.3 logBMP
                         Cloud -> scale 0.3 0.3 cloudBMP
        yOffset = case otype o of
                    Cloud -> 0  
                    _     -> -20 
    in translate (obsX o) (obsY o + yOffset) scaledImage


-- Displays score text in top-left.
drawScore :: Int -> Picture
drawScore s = translate (-500) 350 $ scale 0.3 0.3 $
    text ("Score: " ++ show s)


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

handleEvent _ gs = gs


--	Draws centered black text at given Y coordinate.
drawTextCentered :: String -> Float -> Picture
drawTextCentered txt y =
    translate (-200) y $
        scale 0.5 0.5 $ 
        color black $
            text txt