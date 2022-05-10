module Animation.State
  ( GameState(..)
  , DirectionY(..)
  , DirectionX(..)
  , initGameSate
  , updateState
  ) where

import           Animation.Env                  ( Env(..) )
import           Control.Monad.State.Strict     ( MonadState(get, put)
                                                , MonadTrans(lift)
                                                , StateT(runStateT)
                                                )
import           Control.Monad.Trans.Reader     ( ReaderT(runReaderT)
                                                , ask
                                                )

data DirectionY = GoUp | GoDown deriving (Show, Enum, Eq)

data DirectionX = GoLeft | GoRight deriving (Show, Enum, Eq)

data GameState = GameState
  { boardPos   :: Int
  , ballPos    :: (Int, Int)
  , directionY :: DirectionY
  , directionX :: DirectionX
  , gameStatus :: Bool
  }
  deriving Show


initGameSate = GameState { boardPos   = 0
                         , ballPos    = (5, 5)
                         , directionY = GoUp
                         , directionX = GoRight
                         , gameStatus = True
                         }
type KeyInput = Int

updateState :: KeyInput -> ReaderT Env (StateT GameState IO) ()
updateState keyInput = do
  state <- get
  env   <- ask
  let newState = updateHelper keyInput env state
  lift $ put newState

updateHelper :: KeyInput -> Env -> GameState -> GameState
updateHelper keyInput (Env column row boardSize) (GameState boardPos ballPos directionY directionX gameStatus)
  = GameState { boardPos   = newBoardPos
              , ballPos    = (newPosX, newPosY)
              , directionY = newDirectionY
              , directionX = newDirectionX
              , gameStatus = newGameStatus
              }
 where
  newBoardPos    = boardPos + keyInput -- (-1) left , (1) Right , 0 do nothing
-- New ball position
  (posX, posY) = ballPos
  newDirectionY  = case directionY of
    GoUp -> if posY <= 0 then GoDown else GoUp
    GoDown ->
      if posY == row - 1 && (boardPosLeft <= posX && posX <= boardPosRight)
      then
        GoUp
      else
        GoDown
  newDirectionX = case directionX of
    GoLeft  -> if posX <= 0 then GoRight else GoLeft
    GoRight -> if posX >= (column - 1) then GoLeft else GoRight
  newPosY = if newDirectionY == GoDown then posY + 1 else posY - 1
  newPosX = if newDirectionX == GoRight then posX + 1 else posX - 1
  boardPosRight = newBoardPos + (boardSize `div` 2)
  boardPosLeft =  newBoardPos - (boardSize `div` 2)
  newGameStatus = newPosY < row


-- || posX == (newBoardPos + (boardSize `div` 2)) || posX == (newBoardPos - (boardSize `div` 2))
