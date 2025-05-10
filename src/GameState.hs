--   Defines the overall game state structure, including player (Wolf),
--   list of obstacles, score, game speed, and the current game phase (Start, Playing, GameOver).

module GameState where

import Wolf
import Obstacle
import System.Random (StdGen)


-- Enum for current screen state.
data GamePhase = StartScreen | Playing | GameOver
    deriving (Eq, Show)


-- Holds the entire game state (player, obstacles, score, speed, and current phase.
data GameState = GameState
    { wolf         :: Wolf
    , obstacles    :: [Obstacle]
    , score        :: Int
    , speed        :: Float
    , phase        :: GamePhase
    , frameCounter :: Int
    , rng          :: StdGen
    }



       