module Animation.State
  ( GameState(..)
  , DirectionY(..)
  , DirectionX(..)
  , Object (..)
  , ObjectType (..)
  , initGameSate
  , updateState
  , obListMap
  , calculateDirections
  , calculatePositions
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
  , objectsMap :: Map.Map String Object
  , collisions :: [String]
  , debug :: String
  }
  deriving Show

obListMap :: Map.Map String Object
obListMap = Map.fromList
  [ ("ball", Object (9, 2) True Ball)
  , ("box18", Object (1, 8) False Box)
  , ("box33", Object (3, 3) False Box)
  , ("box37", Object (3, 7) False Box)
  , ("box81", Object (8, 1) False Box)
  , ("box54", Object (5, 4) False Box)
  , ("box16", Object (1, 6) False Box)
  , ("box26", Object (2, 6) False Box)
  , ("box36", Object (3, 6) False Box)
  , ("box46", Object (4, 6) False Box)
  , ("box56", Object (5, 6) False Box)
  , ("box66", Object (6, 6) False Box)
  , ("box76", Object (7, 6) False Box)
  , ("box86", Object (8, 6) False Box)
  , ("box15", Object (1, 5) False Box)
  , ("box25", Object (2, 5) False Box)
  , ("box35", Object (3, 5) False Box)
  , ("box45", Object (4, 5) False Box)
  , ("box55", Object (5, 5) False Box)
  , ("box65", Object (6, 5) False Box)
  , ("box75", Object (7, 5) False Box)
  , ("box85", Object (8, 5) False Box)
  , ("box12", Object (1, 2) False Box)
  , ("box22", Object (2, 2) False Box)
  , ("box32", Object (3, 2) False Box)
  , ("box42", Object (4, 2) False Box)
  , ("box52", Object (5, 2) False Box)
  , ("box62", Object (6, 2) False Box)
  , ("box72", Object (7, 2) False Box)
  , ("box82", Object (8, 2) False Box)]

initGameSate = GameState { boardPos   = 0
                         , ballPos    = (0, 0)
                         , directionY = GoUp
                         , directionX = GoLeft
                         , gameStatus = True
                         , objectsMap = obListMap
                         , collisions = []
                         , debug = ""
                         }
type KeyInput = Int

updateState :: KeyInput -> ReaderT Env (StateT GameState IO) ()
updateState keyInput = do
  state <- get
  env   <- ask
  let newState = updateHelper' keyInput env state
  lift $ put newState


updateHelper' :: KeyInput -> Env -> GameState -> GameState
updateHelper' keyInput env@(Env column row boardSize) state@(GameState boardPos ballPos directionY directionX gameStatus objectsMap collisions lookAheadPos)
  = GameState { boardPos   = newBoardPos
              , ballPos    = (newPosX, newPosY)
              , directionY = newDirectionY'
              , directionX = newDirectionX
              , gameStatus = newGameStatus
              , objectsMap = newObjects
              , collisions = newCollisions
              , debug = mconcat [show newPosX, show newPosY]
              }
 where
  newBoardPos    = boardPos + keyInput -- (-1) left , (1) Right , 0 do nothing
-- New ball position
  (posX, posY) = maybe (error "ball is missing") objectPosition (Map.lookup "ball" objectsMap)
  --boxList = Map.elems $ Map.filterWithKey (\_ value -> not $ isBall value) obListMap

  newDirectionY  = case directionY of
    GoUp -> if posY <= 0
      then GoDown else GoUp
    GoDown ->
      if posY == row-1  && (newBoardPos <= posX && posX <= (newBoardPos + boardSize))
      then
        GoUp
      else
        GoDown
  newDirectionY' = if detectCollision collisions then oppositeY newDirectionY else newDirectionY

  newDirectionX = case directionX of
    GoLeft  -> if posX <= 1  then GoRight else GoLeft
    GoRight -> if posX >= column  then GoLeft else GoRight
  
  newPosY = if newDirectionY' == GoDown then posY + 1 else abs (posY - 1)
  newPosX = if newDirectionX == GoRight then posX + 1 else abs (posX - 1)

  -- collision to look ahead
  newCollisions = Map.keys $ Map.filterWithKey (\key value -> key /= "ball" && (newPosX, newPosY) == objectPosition value ) objectsMap

  newObjectsMap = deleteBoxes newCollisions objectsMap

  newGameStatus = newPosY < row

  newBallObject = Object (newPosX, newPosY) True Ball
  newObjects = Map.adjust (\_ -> newBallObject) "ball" newObjectsMap

calculateDirections :: DirectionX -> DirectionY -> Int -> Int -> Int -> Int -> Int -> Int-> [String]-> (DirectionX, DirectionY)
calculateDirections directionX directionY posX posY row column boardSize boardPos collisions = (newDirectionX, newDirectionY')
  where
    newDirectionY  = case directionY of
      GoUp -> if posY <= 0 -- || not (null collisions)
        then GoDown
         else GoUp
      GoDown ->
        if (posY == row-1  && (boardPos <= posX && posX <= (boardPos + boardSize))) -- || not (null collisions)
          then
            GoUp
            else
        GoDown
    newDirectionX = case directionX of
      GoLeft  -> if posX <= 1  then GoRight else GoLeft
      GoRight -> if posX >= column  then GoLeft else GoRight

    newDirectionY' = if detectCollision collisions then oppositeY newDirectionY else newDirectionY

calculatePositions :: DirectionX -> DirectionY -> Int -> Int -> (Int, Int)
calculatePositions directionX directionY posX posY = (newPosX, newPosY)
  where
    newPosY = if directionY == GoDown then posY + 1 else abs (posY - 1)
    newPosX = if directionX == GoRight then posX + 1 else abs (posX - 1)


deleteBoxes :: Ord k => [k] -> Map.Map k a -> Map.Map k a
deleteBoxes [] obMap = obMap
deleteBoxes (x:xs) obMap = deleteBoxes xs (Map.delete x obMap)

oppositeY :: DirectionY -> DirectionY
oppositeY GoUp = GoDown
oppositeY GoDown = GoUp

oppositeX :: DirectionX -> DirectionX
oppositeX GoRight = GoLeft
oppositeX GoLeft = GoRight

detectCollision :: [String] -> Bool
detectCollision collisions = not (null collisions)