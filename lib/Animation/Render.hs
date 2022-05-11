module Animation.Render
  ( createBoard
  , createBox
  , cleanScreen
  , render
  , rowObjectsToString
  ,renderObject
  ,groupByRowObject
  , render'
  , createEmptySpace'
  , getRowsWithObject
  , listObjects
  , getAllRows
  ) where
import           Animation.Env                  ( Env(..) )
import           Animation.State                ( GameState(..), Object(..) )
import           Control.Monad.State.Strict     ( MonadState(get, put)
                                                , MonadTrans(lift)
                                                , StateT(runStateT)
                                                )
import           Control.Monad.Trans.Reader     ( ReaderT(runReaderT)
                                                , ask
                                                )

import           Data.List                      ( sort
                                                , sortBy
                                                , groupBy
                                                , nub
                                                )
import Data.Function ( on )
import           Debug.Trace
import Prelude hiding (lookup)
import qualified Data.Map as Map (lookup, fromList)




createBall :: Int -> Int -> String
createBall n column =
  mconcat ["|", replicate n ' ', "*", replicate (column - n - 1) ' ', "|"]

createEmptySpace :: Int -> String
createEmptySpace n = mconcat ["|", replicate n ' ', "|"]

createBoard :: Int -> Int -> Int -> String
createBoard n column size = mconcat
  [ "|"
  , replicate n                   ' '
  , replicate size                '='
  , replicate (column - size - n) ' '
  , "|"
  ]

createBox :: Int -> Int -> (Int, Int) -> String
createBox column row (ballPosX, ballPosY) = unlines boxList
 where
  top     = replicate ballPosY (createEmptySpace column)
  middle  = [createBall ballPosX column]
  bottom  = replicate (row - ballPosY - 1) (createEmptySpace column)
  boxList = mconcat [top, middle, bottom]

createBall' :: [Int] -> [String]
createBall' list = ["|", renderedRow, filledSpace, "|"]
 where
  sortedList            = sort list
  distanceBetweenPoints = zipWith (-) sortedList (0 : init sortedList)
  renderedRow =
    mconcat $ map (\x -> replicate (x - 1) ' ' ++ "=") distanceBetweenPoints
  filledSpace = mconcat $ replicate (10 - last sortedList) " "

rowObjectsToString :: Maybe [Object] -> String
rowObjectsToString (Just list) = mconcat ["|", renderedRow, filledSpace, "|"]
 where
  -- Sort objects by x position
  getColumns = map (\(Object (x,_) isBall)-> (x,isBall)) list
  sortedByXPos = sortBy (compare `on`fst) getColumns
  --objectXPos (Object (x, _) _) (Object (x', _) _) = compare x x'

  -- Calculate distance between x positions
  distBetweenXPos = zipWith (\(x,isBall) (x',_)-> (x-x',isBall) ) sortedByXPos sortedByXPosOffSet
  --substracXPos (Object (x,y) isBall) (Object (x',_) _) = Object (x-x',y) isBall
  sortedByXPosOffSet = (0,False) : init sortedByXPos
  --sortedByXPosOffSet = Object (0, 0) False : init sortedByXPos

  -- With distance between positions calculated, then render object based on its type
  renderedRow = mconcat $ map renderRow distBetweenXPos
  --renderRow (Object (x, y) isBall) = replicate (x - 1) ' ' ++ if isBall then "*" else "="
  renderRow (x,isBall) = replicate (x-1) ' ' ++ if isBall then "*" else "="

  -- get x positions from sorted object to  render remaining space
  positionXObjects            = map fst sortedByXPos
  filledSpace = mconcat $ replicate (10 - last positionXObjects) " "
rowObjectsToString Nothing = createEmptySpace' 10


createEmptySpace' :: Int -> String
createEmptySpace' n = mconcat ["|", replicate n ' ', "|"]

-- Group object based on position y
groupByRowObject :: [Object] -> [[Object]]
groupByRowObject = groupBy (\(Object (_,y) _) (Object (_,y') _) -> y==y')

getRowsWithObject :: [Object] -> [Int]
getRowsWithObject list = nub $ map snd $ mconcat $ map (\x-> map objectPosition x) (groupByRowObject list)

listObjects :: [Object] -> [(Int, [Object])]
listObjects list = zip (getRowsWithObject list) (groupByRowObject list)

getAllRows :: [Object] -> [Maybe [Object]]
getAllRows list = map (\x-> Map.lookup x (Map.fromList (listObjects list))) [1 .. 10]

-- Render list of objects when these are grouped by y position
renderObject :: [Object] -> String
renderObject list = unlines $ map rowObjectsToString (getAllRows list)

toListColumns :: [Object] -> [(Int, Bool)]
toListColumns = map (\(Object (x,_) isBall)-> (x,isBall))

cleanScreen :: IO ()
cleanScreen = putStr "\ESC[2J"

render :: ReaderT Env (StateT GameState IO) ()
render = do
  state <- get
  env   <- ask
  game  <- renderHelper state env
  --lift $ lift cleanScreen
  lift $ lift $ putStrLn $ game ++ "\n"

render' :: ReaderT Env (StateT GameState IO) ()
render' = do
  state <- get
  env <- ask
  game <- renderHelper' state env
  lift $ lift $ putStrLn $ game ++ "\n"

renderHelper' :: GameState -> Env -> ReaderT Env (StateT GameState IO) String
renderHelper' state env =  do
  let board = createBoard (boardPos state) (column env) (boardSize env)
  let newBox = renderObject (objects state)
  return (mconcat [newBox, board, show $ ballPos state
      , show $ boardPos state
      , show $ gameStatus state
      , show $ objects state])

renderHelper :: GameState -> Env -> ReaderT Env (StateT GameState IO) String
renderHelper state env = do
  let board  = createBoard (boardPos state) (column env) (boardSize env)
  let newBox = createBox (column env) (row env) (ballPos state)
  return
    (mconcat
      [ newBox
      , board
      , show $ ballPos state
      , show $ boardPos state
      , show $ gameStatus state
      ]
    )
