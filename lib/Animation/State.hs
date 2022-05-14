module Animation.State
  ( GameState(..)
  , DirectionY(..)
  , DirectionX(..)
  , Object (..)
  , ObjectType (..)
  , initGameSate
  , updateState
  , obList
  ) where

import           Animation.Env                  ( Env(..) )
import           Control.Monad.State.Strict     ( MonadState(get, put)
                                                , MonadTrans(lift)
                                                , StateT(runStateT)
                                                )
import           Control.Monad.Trans.Reader     ( ReaderT(runReaderT)
                                                , ask
                                                )

import qualified Data.Map.Strict as Map

data DirectionY = GoUp | GoDown deriving (Show, Enum, Eq)

data DirectionX = GoLeft | GoRight deriving (Show, Enum, Eq)

data ObjectType = Ball | Box deriving Show

data Object = Object
  { objectPosition :: (Int, Int)
  , isBall         :: Bool
  , objectType :: ObjectType
  }
  deriving Show

data GameState = GameState
  { boardPos   :: Int
  , ballPos    :: (Int, Int)
  , directionY :: DirectionY
  , directionX :: DirectionX
  , gameStatus :: Bool
  , objects :: [Object]
  , objectsMap :: Map.Map Int Object
  }
  deriving Show

obList :: [Object]
obList =
  [-- Object (1, 1) False Box
  Object (9, 2) True Ball
  --, Object (5, 2) False Box
  --, Object (4, 8) False Box
  --, Object (7, 8) False Box
  --, Object (9, 8) False Box
  --, Object (3, 8) False Box
  , Object (1, 8) False Box
  ]

obListMap :: Map.Map Int Object
obListMap = Map.fromList $ zip [1 .. length obList] obList

initGameSate = GameState { boardPos   = 0
                         , ballPos    = (0, 0)
                         , directionY = GoUp
                         , directionX = GoRight
                         , gameStatus = True
                         , objects = obList
                         , objectsMap = obListMap
                         }
type KeyInput = Int

updateState :: KeyInput -> ReaderT Env (StateT GameState IO) ()
updateState keyInput = do
  state <- get
  env   <- ask
  let newState = updateHelper' keyInput env state
  lift $ put newState

updateHelper :: KeyInput -> Env -> GameState -> GameState
updateHelper keyInput (Env column row boardSize) (GameState boardPos ballPos directionY directionX gameStatus objects objectsMap)
  = GameState { boardPos   = newBoardPos
              , ballPos    = (newPosX, newPosY)
              , directionY = newDirectionY
              , directionX = newDirectionX
              , gameStatus = newGameStatus
              , objects = objects
              , objectsMap = objectsMap
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

updateHelper' :: KeyInput -> Env -> GameState -> GameState
updateHelper' keyInput (Env column row boardSize) (GameState boardPos ballPos directionY directionX gameStatus objects objectsMap)
  = GameState { boardPos   = newBoardPos
              , ballPos    = (newPosX, newPosY)
              , directionY = newDirectionY
              , directionX = newDirectionX
              , gameStatus = newGameStatus
              , objects = newObjects
              , objectsMap = objectsMap
              }
 where
  newBoardPos    = boardPos + keyInput -- (-1) left , (1) Right , 0 do nothing
-- New ball position
  (posX, posY) = head $ map objectPosition $ filter isBall objects
  detectCollision = or $ map (\x -> (posX, posY) == objectPosition x ) $ filter (not . isBall) objects
  newBoxes = if detectCollision then [] else filter (not . isBall) objects
  newDirectionY  = case directionY of
    GoUp -> if posY <= 0  then GoDown else GoUp
    GoDown ->
      if (posY == row - 1 && (newBoardPos <= posX && posX <= (newBoardPos + boardSize))) || detectCollision
      then
        GoUp
      else
        GoDown
  newDirectionX = case directionX of
    GoLeft  -> if posX <= 1  then GoRight else GoLeft
    GoRight -> if posX >= column then GoLeft else GoRight
  newPosY = if newDirectionY == GoDown then posY + 1 else posY - 1
  newPosX = if newDirectionX == GoRight then posX + 1 else posX - 1
  boardPosRight = newBoardPos + (boardSize `div` 2)
  boardPosLeft =  newBoardPos - (boardSize `div` 2)


  newGameStatus = newPosY < row

  newBallObject = Object (newPosX, newPosY) True Ball
  newObjects = newBallObject : newBoxes