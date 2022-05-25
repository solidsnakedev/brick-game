module Animation.State
  ( GameState(..)
  , DirectionY(..)
  , DirectionX(..)
  , Object (..)
  , ObjectType (..)
  , UserInput (..)
  , GameStatus (..)
  , initGameState
  , updateState
  , mkRandObjects
  , parseUserInput
  , checkGameStatus
  , checkBoardBoundaries
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


data UserInput = MoveLeft | MoveRight | Quit | InvalidInput | NoInput deriving Show

data GameStatus = Running | EndGame deriving (Show, Eq)

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
  , gameStatus :: GameStatus
  , objectsMap :: Map.Map String Object
  , collisions :: [String]
  , score :: Int
  , debug :: String
  }
  deriving Show


getRandPos :: IO (Int, Int)
getRandPos = do
  randX <- randomRIO (1,30) :: IO Int
  randY <- randomRIO (0,10) :: IO Int
  return (randX, randY)

mkRandObjects :: IO (Map.Map String Object)
mkRandObjects = do
  brickList <- mapM (\_-> getRandPos) [1..70]
  let ob = map (\x -> Object x Brick) brickList
  let bricks = map (\object -> ("brick" ++ show (objectPosition object), object)) ob
  ballPos <- getRandPos
  let ball = Object ballPos Ball
  let objects = Map.fromList $ ("ball", ball) : bricks
  print objects
  return objects


initGameState = do
    objects <- mkRandObjects
    return $ GameState { boardPos   = 0
                         , directionY = GoUp
                         , directionX = GoLeft
                         , gameStatus = Running
                         , objectsMap = objects
                         , collisions = []
                         , score = 0
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
updateStateHelper keyInput env@(Env column row boardSize) state@(GameState boardPos directionY directionX gameStatus objectsMap collisions score debug)
  = GameState { boardPos   = newBoardPos
              , directionY = newDirectionY
              , directionX = newDirectionX
              , gameStatus = newGameStatus
              , objectsMap = newObjects
              , collisions = newCollisions
              , score = newScore
              , debug = mconcat [ show newPosX, " "
                                , show newPosY, " "
                                , show newDirectionX, " "
                                , show newDirectionY, " "
                                , show newCollisions
                                , show boardPos]
              }
 where
  -- Key input reference : (-1) left , (1) Right , 0 do nothing
  newBoardPos    = checkBoardBoundaries boardPos boardSize keyInput column
    -- boardPos + keyInput
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

  newScore = if isCollision then score + 1 else score

  -- Create ball with new positions
  newBall = Object (newPosX, newPosY) Ball
  -- Update ball positions
  newObjects = Map.adjust (\_ -> newBall) "ball" newObjectsMap

  -- Finally check if ball went beyond row position
  newGameStatus = if newPosY > row then EndGame else Running

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

parseUserInput :: Maybe Char -> UserInput
parseUserInput (Just userInput) =
  case userInput of
    'a' -> MoveLeft
    'd' -> MoveRight
    'q' -> Quit
    _   -> InvalidInput
parseUserInput Nothing = NoInput

checkGameStatus :: ReaderT Env (StateT GameState IO) () -> String -> ReaderT Env (StateT GameState IO) ()
checkGameStatus returnFuction exitMessage = do
  state <- lift get
  if gameStatus state == Running then
    returnFuction
    else lift $ lift $ putStrLn exitMessage

checkBoardBoundaries :: (Ord a, Num a) => a -> a -> a -> a -> a
checkBoardBoundaries boardPos boardSize keyInput column
  | boardPos + keyInput >= 0 && boardPos + boardSize + keyInput <= column = boardPos + keyInput
  | boardPos + keyInput < 0 = 0
  | otherwise = column - boardSize