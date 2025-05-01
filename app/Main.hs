-- | Entry point for the Wolf Run game.
--   Initializes the window, sets up the game loop, and starts rendering and input handling.

module Main where
import Wolf (defaultWolf)
import Brillo
import Brillo.Interface.IO.Game (playIO)
import GameLoop
import GameState
import qualified Data.Text as T  -- Add this import

main :: IO ()
main = playIO
  (InWindow (T.pack "Wolf Run") (1200, 800) (100, 100))  -- Convert String to Text
  white
  60
  initialState
  (return . drawGame)  -- This is fine
  (\e gs -> return (handleEvent e gs))  -- Wrap in IO
  (\dt gs -> return (gameStep dt gs))   -- Fix application and wrap in IO
  
-- Initial game state
initialState :: GameState
initialState = GameState
  { wolf = defaultWolf
  , obstacles = []
  , score = 0
  , speed = 5
  , phase = StartScreen
  }