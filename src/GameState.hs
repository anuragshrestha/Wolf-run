--   Defines the overall game state structure, including player (Wolf),
--   list of obstacles, score, game speed, and the current game phase (Start, Playing, GameOver).

module GameState where

import Wolf
import Obstacle


data GamePhase = StartScreen | Playing | GameOver
    deriving (Eq, Show)

data GameState = GameState
    { wolf      :: Wolf
    , obstacles :: [Obstacle]
    , score     :: Int
    , speed     :: Float
    , phase     :: GamePhase
    } deriving (Show)

       