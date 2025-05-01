--   Represents the player character (the wolf), including its position,
--   vertical velocity, and status (Running, Jumping, Docking).
--   Also defines default wolf parameters and motion constants.

module Wolf where

data WolfStatus = Running | Jumping | Docking
    deriving (Eq, Show)

data Wolf = Wolf
    { wolfX      :: Float
    , wolfY      :: Float
    , velocityY  :: Float
    , status     :: WolfStatus
    } deriving (Show)

-- Constants for motion (to be tuned)
jumpVelocity :: Float
jumpVelocity = 10

gravity :: Float
gravity = -0.5

defaultWolf :: Wolf
defaultWolf = Wolf 100 0 0 Running