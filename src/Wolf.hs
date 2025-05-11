--   Represents the player character (the wolf), including its position,
--   vertical velocity, and status (Running, Jumping, Docking).
--   Also defines default wolf parameters and motion constants.

module Wolf where


-- Enum to track wolf’s movement status
data WolfStatus = Running | Jumping | Docking
    deriving (Eq, Show)


-- Represents the wolf’s position, vertical motion, and current state.
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


--The wolf starts at x = 100, y = 0, no vertical motion, and is running.
defaultWolf :: Wolf
defaultWolf = Wolf 30 0 0 Running