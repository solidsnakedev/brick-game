module Animation.Render (createBoard, createBox, cleanScreen, render)where
import Animation.State (GameState (..))
import Animation.Env (Env(..))
import           Control.Monad.State.Strict     ( MonadState(get, put)
                                                , MonadTrans(lift)
                                                , StateT(runStateT)
                                                )
import           Control.Monad.Trans.Reader     ( ReaderT(runReaderT)
                                                , ask
                                                )

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
renderHelper state env = do
  let board        = createBoard (boardPos state) (column env) (boardSize env)
  let newBox  = createBox (column env) (row env) (ballPos state)
  return (mconcat [newBox, board, show $ ballPos state , show $ boardPos state, show $ gameStatus state])
