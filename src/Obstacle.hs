--  Defines obstacle types (Log, Rock, Cloud) and their structure.
--   Each obstacle has a position and a type for rendering and interaction logic.

module Obstacle where


-- Defines the Enum of obstacle types.
data ObstacleType = Log | Rock | Cloud
    deriving (Eq, Show)


-- 	Represents an obstacle with position and type.
data Obstacle = Obstacle
    { obsX  :: Float
    , obsY  :: Float
    , otype :: ObstacleType
    } deriving (Show)