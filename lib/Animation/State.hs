module Animation.State
  ( GameState(..)
  , DirectionY(..)
  , DirectionX(..)
  , initGameSate
  , animationHelper
  ) where

import           Control.Monad.Trans.Reader     ( ReaderT(runReaderT)
                                                , ask
                                                )
import           Control.Monad.State.Strict     ( MonadState(get, put)
                                                , MonadTrans(lift)
                                                , StateT(runStateT)
                                                )
import  Animation.Env (Env(..))

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


initGameSate = GameState { box        = ""
                         , board      = ""
                         , boardPos   = 0
                         , ballPos    = (5, 5)
                         , directionY = GoUp
                         , directionX = GoRight
                         }
type KeyInput = Char

animationHelper :: Int -> ReaderT Env (StateT GameState IO) ()
animationHelper move = do
  state <- get
  env   <- ask

  let boardPos' = boardPos state + move -- (-1) left , (1) Right , 0 do nothing

  -- New ball position
  let (posX, posY) = ballPos state
  let directionY' = case directionY state of
        GoUp   -> if posY <= 0 then GoDown else GoUp
        GoDown -> if posY >= (row env - 1) then GoUp else GoDown
  let directionX' = case directionX state of
        GoLeft  -> if posX <= 0 then GoRight else GoLeft
        GoRight -> if posX >= (column env - 1) then GoLeft else GoRight
  let posY' = if directionY' == GoDown then posY + 1 else posY - 1
  let posX' = if directionX' == GoRight then posX + 1 else posX - 1

  lift $ put $ GameState { box        = ""
                  , board      = ""
                  , boardPos   = boardPos'
                  , ballPos    = (posX', posY')
                  , directionY = directionY'
                  , directionX = directionX'
                  }
