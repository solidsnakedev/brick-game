module Animation.State
  ( GameState(..)
  , DirectionY(..)
  , DirectionX(..)
  , Object (..)
  , ObjectType (..)
  , initGameSate
  , updateState
  , obMap
  , mkRandObjects
  ) where

import           Animation.Env                  ( Env(..) )
import           Control.Monad.State.Strict     ( MonadState(get, put)
                                                , MonadTrans(lift)
                                                , StateT(runStateT)
                                                )
import           Control.Monad.Trans.Reader     ( ReaderT(runReaderT)
                                                , ask
                                                )
import System.Random

import qualified Data.Map.Strict as Map

data DirectionY = GoUp | GoDown deriving (Show, Enum, Eq)

data DirectionX = GoLeft | GoRight deriving (Show, Enum, Eq)

data ObjectType = Ball | Brick deriving Show

data Object = Object
  { objectPosition :: (Int, Int)
  , objectType  :: ObjectType
  }
  deriving Show

data GameState = GameState
  { boardPos   :: Int
  , directionY :: DirectionY
  , directionX :: DirectionX
  , gameStatus :: Bool
  , objectsMap :: Map.Map String Object
  , collisions :: [String]
  , debug :: String
  }
  deriving Show


getRandomObjects :: IO (Int, Int)
getRandomObjects = do
  randX <- randomRIO (1,10) :: IO Int
  randY <- randomRIO (0,5) :: IO Int
  return (randX, randY)

mkRandObjects :: IO (Map.Map String Object)
mkRandObjects = do
  brickList <- mapM (\_-> getRandomObjects) [1..30]
  let ob = map (\x -> Object x Brick) brickList
  let bricks = map (\object -> ("brick" ++ show (objectPosition object), object)) ob
  ballPos <- getRandomObjects
  let ball = Object ballPos Ball
  let objects = Map.fromList $ ("ball", ball) : bricks
  print objects
  return objects


obMap :: Map.Map String Object
obMap = Map.fromList
  [ ("ball", Object (9, 2) Ball)
  , ("box18", Object (1, 8) Brick)
  , ("box33", Object (3, 3) Brick)
  , ("box37", Object (3, 7) Brick)
  , ("box81", Object (8, 1) Brick)
  , ("box54", Object (5, 4) Brick)
  , ("box16", Object (1, 6) Brick)
  , ("box26", Object (2, 6) Brick)
  , ("box36", Object (3, 6) Brick)
  , ("box46", Object (4, 6) Brick)
  , ("box56", Object (5, 6) Brick)
  , ("box66", Object (6, 6) Brick)
  , ("box76", Object (7, 6) Brick)
  , ("box86", Object (8, 6) Brick)
  , ("box15", Object (1, 5) Brick)
  , ("box25", Object (2, 5) Brick)
  , ("box35", Object (3, 5) Brick)
  , ("box45", Object (4, 5) Brick)
  , ("box55", Object (5, 5) Brick)
  , ("box65", Object (6, 5) Brick)
  , ("box75", Object (7, 5) Brick)
  , ("box85", Object (8, 5) Brick)
  , ("box12", Object (1, 2) Brick)
  , ("box22", Object (2, 2) Brick)
  , ("box32", Object (3, 2) Brick)
  , ("box42", Object (4, 2) Brick)
  , ("box52", Object (5, 2) Brick)
  , ("box62", Object (6, 2) Brick)
  , ("box72", Object (7, 2) Brick)
  , ("box82", Object (8, 2) Brick)
  , ("box10", Object (1, 0) Brick)
  , ("box20", Object (2, 0) Brick)
  , ("box30", Object (3, 0) Brick)
  , ("box40", Object (4, 0) Brick)
  , ("box50", Object (5, 0) Brick)
  , ("box60", Object (6, 0) Brick)
  , ("box70", Object (7, 0) Brick)
  , ("box80", Object (8, 0) Brick)]

initGameSate = do
    objects <- mkRandObjects
    return $ GameState { boardPos   = 0
                         , directionY = GoUp
                         , directionX = GoLeft
                         , gameStatus = True
                         , objectsMap = objects
                         , collisions = []
                         , debug = ""
                         }
type KeyInput = Int


updateState :: KeyInput -> ReaderT Env (StateT GameState IO) ()
updateState keyInput = do
  state <- get
  env   <- ask
  let newState = updateStateHelper keyInput env state
  lift $ put newState


updateStateHelper :: KeyInput -> Env -> GameState -> GameState
updateStateHelper keyInput env@(Env column row boardSize) state@(GameState boardPos directionY directionX gameStatus objectsMap collisions lookAheadPos)
  = GameState { boardPos   = newBoardPos
              , directionY = newDirectionY
              , directionX = newDirectionX
              , gameStatus = newGameStatus
              , objectsMap = newObjects
              , collisions = newCollisions
              , debug = mconcat [ show newPosX, " "
                                , show newPosY, " "
                                , show newDirectionX, " "
                                , show newDirectionY, " "
                                , show newCollisions]
              }
 where
  -- Key input reference : (-1) left , (1) Right , 0 do nothing
  newBoardPos    = boardPos + keyInput
  -- Get ball position
  (posX, posY) = maybe (error "ball is missing") objectPosition (Map.lookup "ball" objectsMap)

  isCollision = not (null collisions)
  isBoardAligned = newBoardPos <= posX && posX <= (newBoardPos + boardSize)
  -- Get new Y direction, based on either ball position and row borders or any collision
  newDirectionY  = calcDirectionY directionY posY row isBoardAligned isCollision

  -- Get new X direction, based on ball position  and column borders
  newDirectionX = calcDirectionX directionX posX column

  -- Get Y position based on new Y direction
  newPosY = calcPosY newDirectionY posY
  -- Get X position based on new X direction
  newPosX = calcPosX newDirectionX posX

  -- Detect collision from Objects
  newCollisions = Map.keys $ Map.filterWithKey (\key value -> key /= "ball" && (newPosX, newPosY) == objectPosition value ) objectsMap
  -- If collision then delete Brick
  newObjectsMap = deleteBricks newCollisions objectsMap

  -- Create ball with new positions
  newBall = Object (newPosX, newPosY) Ball
  -- Update ball positions
  newObjects = Map.adjust (\_ -> newBall) "ball" newObjectsMap

  -- Finally check if ball when beyond row position
  newGameStatus = newPosY < row

calcDirectionY :: (Ord a, Num a) => DirectionY -> a -> a -> Bool -> Bool -> DirectionY
calcDirectionY directionY posY row isBoardAligned isCollision =
  case directionY of
    GoUp -> if posY <= 0 || isCollision then GoDown else GoUp
    GoDown -> if posY == row-1  && isBoardAligned || isCollision then GoUp else GoDown

calcDirectionX :: (Ord a, Num a) => DirectionX -> a -> a -> DirectionX
calcDirectionX directionX posX column =
  case directionX of
    GoLeft  -> if posX <= 1  then GoRight else GoLeft
    GoRight -> if posX >= column  then GoLeft else GoRight

calcPosX :: Num a => DirectionX -> a -> a
calcPosX directionX posX = if directionX == GoRight then posX + 1 else abs (posX - 1)

calcPosY :: Num a => DirectionY -> a -> a
calcPosY directionY posY = if directionY == GoDown then posY + 1 else abs (posY - 1)


deleteBricks :: Ord k => [k] -> Map.Map k a -> Map.Map k a
deleteBricks [] obMap = obMap
deleteBricks (x:xs) obMap = deleteBricks xs (Map.delete x obMap)