module Animation.Render
  ( createBoard
  , cleanScreen
  , rowsToString
  , renderObject
  , groupByRowObject
  , render
  , createEmptySpace
  , getRowsWithObject
  , mkRowsWithObjects
  , sortObjects
  ) where
import           Animation.Env                  ( Env(..) )
import           Animation.State                ( GameState(..), Object(..), ObjectType (..) )
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
import qualified Data.Map.Strict as Map

createBoard :: Int -> Int -> Int -> String
createBoard n column size = mconcat
  [ "|"
  , replicate n                   ' '
  , replicate size                ':'
  , replicate (column - size - n) ' '
  , "|"
  ]

createEmptySpace :: Int -> String
createEmptySpace n = mconcat ["|", replicate n ' ', "|"]

rowsToString :: Int ->  [Object] -> String
rowsToString column objects
  | null objects = createEmptySpace column
  | otherwise = mconcat ["|", renderedRow, fillSpace, "|"]
    where
      -- Sort objects by x position
      getXPosAndType = map (\x-> (fst $ objectPosition x, objectType x)) objects
      sortedByXPos = sortBy (compare `on`fst) getXPosAndType
      --objectXPos (Object (x, _) _) (Object (x', _) _) = compare x x'

      -- Calculate distance between x positions
      distBetweenXPos = zipWith (\(x,objectType) x'-> (x-x',objectType) ) sortedByXPos sortedByXPosOffSet
      --substracXPos (Object (x,y) isBall) (Object (x',_) _) = Object (x-x',y) isBall
      sortedByXPosOffSet = 0: init (map fst sortedByXPos)
      --sortedByXPosOffSet = Object (0, 0) False : init sortedByXPos

      -- With distance between positions calculated, then render object based on its type
      renderedRow = mconcat $ map renderRow distBetweenXPos
      --renderRow (Object (x, y) isBall) = replicate (x - 1) ' ' ++ if isBall then "*" else "="
      renderRow (x,objectType) = replicate (x-1) ' ' ++ checkBallBrick objectType

      checkBallBrick objectType =
        case objectType of
          Ball -> "o"
          Brick -> "="

      -- get x positions from sorted object to  render remaining space
      onlyXPositions            = map fst sortedByXPos
      fillSpace = mconcat $ replicate (column - last onlyXPositions) " "


sortObjects :: [Object] -> [Object]
sortObjects = sortBy (compare `on` snd . objectPosition)
-- Group object based on position y
groupByRowObject :: [Object] -> [[Object]]
groupByRowObject = groupBy (\x y -> snd (objectPosition x) == snd (objectPosition y) )

getRowsWithObject :: [Object] -> [Int]
getRowsWithObject = nub . map (snd . objectPosition)

mkRowsWithObjects :: [Object] -> [(Int, [Object])]
mkRowsWithObjects objects = zip (getRowsWithObject objects) (groupByRowObject objects)

mkRows :: Int -> [Object] -> [[Object]]
mkRows row ob = Map.elems $ Map.union rowsWithObjectsMap dummyMap
  where
    sortedObjects = sortObjects ob
    rowsWithObjects = mkRowsWithObjects sortedObjects
    rowsWithObjectsMap = Map.fromList rowsWithObjects
    dummyMap = Map.fromList $ (\x-> (x,[])) <$> [0 .. row - 1]

-- Render list of objects when these are grouped by y position
renderObject :: [Object] -> Int -> Int -> String
renderObject objects row column = unlines $ zipWith (++) (map (rowsToString column) (mkRows row objects)) (show <$> [0..row -1]) 

cleanScreen :: IO ()
cleanScreen = putStr "\ESC[2J"

render :: ReaderT Env (StateT GameState IO) ()
render = do
  state <- get
  env <- ask
  game <- renderHelper state env
  --lift $ lift cleanScreen
  lift $ lift $ putStrLn $ game ++ "\n"

renderHelper :: GameState -> Env -> ReaderT Env (StateT GameState IO) String
renderHelper state env =  do
  let board = createBoard (boardPos state) (column env) (boardSize env)
  --let (ballPosX, ballPosY) = Map.filter "ball" (objectsMap state)
  let newBox = renderObject (Map.elems $ objectsMap state) (row env) (column env)
  return (mconcat [ newBox
                  , board,"\n"
                  , "SCORE: "
                  , show $ score state, "\n"
                  , "Debug: "
                  , debug state
                  ])
