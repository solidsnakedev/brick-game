module Animation.State
  ( GameState(..)
  , DirectionY(..)
  , DirectionX(..)
  ) where

data DirectionY = GoUp | GoDown deriving (Show, Enum, Eq)
data DirectionX = GoLeft | GoRight deriving (Show, Enum, Eq)

data GameState = GameState
  { box        :: String
  , board      :: String
  , boardPos   :: Int
  , ballPos    :: (Int, Int)
  , directionY :: DirectionY
  , directionX :: DirectionX
  }
  deriving Show