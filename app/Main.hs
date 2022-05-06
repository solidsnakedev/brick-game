module Main where
import           Animation.State                ( DirectionX(..)
                                                , DirectionY(..)
                                                , GameState(..)
                                                )
import           Control.Concurrent             ( forkIO
                                                , newEmptyMVar
                                                , putMVar
                                                , threadDelay
                                                , tryTakeMVar
                                                )
import           Control.Monad.State.Strict     ( MonadState(get, put)
                                                , MonadTrans(lift)
                                                , StateT(runStateT)
                                                )
import           Control.Monad.Trans.Maybe      ( )
import           Control.Monad.Trans.Reader     ( ReaderT(runReaderT)
                                                , ask
                                                )
import           Data.List
import           System.IO                      ( BufferMode(..)
                                                , hFlush
                                                , hSetBuffering
                                                , hSetEcho
                                                , stdin
                                                , stdout
                                                )

createBall :: Int -> Int -> [Char]
createBall n column =
  mconcat ["|", replicate n ' ', "*", replicate (column - n - 1) ' ', "|"]

createEmptySpace :: Int -> [Char]
createEmptySpace n = mconcat ["|", replicate n ' ', "|"]

createBoard :: Int -> Int -> Int -> [Char]
createBoard n column size = mconcat
  [ "|"
  , replicate n                   ' '
  , replicate size                '='
  , replicate (column - size - n) ' '
  , "|"
  ]

createBox :: Int -> Int -> (Int, Int) -> [String]
createBox column row (ballPosX, ballPosY) = mconcat
  [ replicate ballPosY (createEmptySpace column)
  , [createBall ballPosX column]
  , replicate (row - ballPosY - 1) (createEmptySpace column)
  ]

getCommnads :: Char -> IO ()
getCommnads c | c == 'w'  = putStrLn $ unlines $ createBox 10 10 (7, 4)
              | c == 's'  = putStrLn $ unlines $ createBox 10 10 (7, 4)
              | c == 'a'  = putStrLn $ unlines $ createBox 10 10 (7, 4)
              | c == 'd'  = putStrLn $ unlines $ createBox 10 10 (7, 4)
              | otherwise = putStrLn "wrong input"


initGameSate = GameState { box        = ""
                         , board      = ""
                         , boardPos   = 0
                         , ballPos    = (5, 5)
                         , directionY = GoUp
                         , directionX = GoRight
                         }

data Env = Env
  { column    :: Int
  , row       :: Int
  , boardSize :: Int
  }
  deriving Show
initEnv = Env 10 10 3

clean :: IO ()
clean = putStr "\ESC[2J"

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations n xs =
  [ y : ys | y : xs' <- tails xs, ys <- combinations (n - 1) xs' ]

mainGame :: ReaderT Env (StateT GameState IO) String
mainGame = do

  e      <- ask
  st     <- get
  newVar <- lift $ lift newEmptyMVar

  lift $ lift $ forkIO
    (do
      a <- getChar
      putChar '\n'
      putMVar newVar a
    )

  wait newVar e st

 where
  wait threadVar env state = do
    lift $ lift $ threadDelay (1000000 `div` 3)
    mVar <- lift $ lift $ tryTakeMVar threadVar
    case mVar of
      Just var -> case var of
        'a' -> do
          -- Render Board
          let p            = boardPos state - 1
          let board        = createBoard p (column env) (boardSize env)

          -- Render Box
          let (posX, posY) = ballPos state
          let directionY' = case directionY state of
                GoUp   -> if posY <= 0 then GoDown else GoUp
                GoDown -> if posY >= (row env - 1) then GoUp else GoDown
          let directionX' = case directionX state of
                GoLeft  -> if posX <= 0 then GoRight else GoLeft
                GoRight -> if posX >= (column env - 1) then GoLeft else GoRight
          let posY' = if directionY' == GoDown then posY + 1 else posY - 1
          let posX' = if directionX' == GoRight then posX + 1 else posX - 1
          let box'  = unlines $ createBox (column env) (row env) (posX', posY')

          -- update State
          put $ GameState { box        = ""
                          , board      = ""
                          , boardPos   = p
                          , ballPos    = (posX', posY')
                          , directionY = directionY'
                          , directionX = directionX'
                          }
          lift $ lift clean
          -- Print board and box
          lift $ lift $ putStrLn (box' ++ "\n" ++ board)
          -- return to Game
          mainGame
        'd' -> do
          -- Render Board
          let p            = boardPos state + 1
          let board        = createBoard p (column env) (boardSize env)

          -- Render Box
          let (posX, posY) = ballPos state
          let directionY' = case directionY state of
                GoUp   -> if posY <= 0 then GoDown else GoUp
                GoDown -> if posY >= (row env - 1) then GoUp else GoDown
          let directionX' = case directionX state of
                GoLeft  -> if posX <= 0 then GoRight else GoLeft
                GoRight -> if posX >= (column env - 1) then GoLeft else GoRight
          let posY' = if directionY' == GoDown then posY + 1 else posY - 1
          let posX' = if directionX' == GoRight then posX + 1 else posX - 1
          let box'  = unlines $ createBox (column env) (row env) (posX', posY')

          -- update State
          put $ GameState { box        = ""
                          , board      = ""
                          , boardPos   = p
                          , ballPos    = (posX', posY')
                          , directionY = directionY'
                          , directionX = directionX'
                          }
          -- Print board and box
          lift $ lift clean
          lift $ lift $ putStrLn (box' ++ "\n" ++ board)
          mainGame
        'q' -> do
          return ""
        _ -> do
          let board = createBoard (boardPos state) (column env) (boardSize env)
          let box'  = unlines $ createBox (column env) (row env) (ballPos state)
          lift $ lift clean
          lift $ lift $ putStrLn (box' ++ "\n" ++ board)
          --lift $ lift $ threadDelay 100000
          mainGame
      Nothing -> do
                -- Render Box
        let (posX, posY) = ballPos state
        let directionY' = case directionY state of
              GoUp   -> if posY <= 0 then GoDown else GoUp
              GoDown -> if posY >= (row env - 1) then GoUp else GoDown
        let directionX' = case directionX state of
              GoLeft  -> if posX <= 0 then GoRight else GoLeft
              GoRight -> if posX >= (column env - 1) then GoLeft else GoRight
        let posY' = if directionY' == GoDown then posY + 1 else posY - 1
        let posX' = if directionX' == GoRight then posX + 1 else posX - 1

        let box'  = unlines $ createBox (column env) (row env) (posX', posY')
        -- update State
        put $ GameState { box        = ""
                        , board      = ""
                        , boardPos   = boardPos state
                        , ballPos    = (posX', posY')
                        , directionY = directionY'
                        , directionX = directionX'
                        }

        let board = createBoard (boardPos state) (column env) (boardSize env)
        lift $ lift clean
        lift $ lift $ putStrLn (box' ++ "\n" ++ board)

        newState <- get
        wait threadVar env newState


ballGame :: ReaderT Env (StateT GameState IO) String
ballGame = do
    --lift $ lift $ threadDelay 1000000
  e  <- ask
  st <- get
  let (posX, posY) = ballPos st
  let directionY' = case directionY st of
        GoUp   -> if posY <= 0 then GoDown else GoUp
        GoDown -> if posY >= (row e - 1) then GoUp else GoDown
  let directionX' = case directionX st of
        GoLeft  -> if posX <= 0 then GoRight else GoLeft
        GoRight -> if posX >= (column e - 1) then GoLeft else GoRight
  let posY' = if directionY' == GoDown then posY + 1 else posY - 1
  let posX' = if directionX' == GoRight then posX + 1 else posX - 1
  let box'  = unlines $ createBox (column e) (row e) (posX', posY')
  lift $ put GameState { box        = box'
                       , board      = board st
                       , boardPos   = boardPos st
                       , ballPos    = (posX', posY')
                       , directionY = directionY'
                       , directionX = directionX'
                       }
  --lift $ lift $ putStrLn box'
  return box'


main = do
  --hSetEcho stdin False
  runStateT (runReaderT mainGame initEnv) initGameSate

callGame initEnv initGameState = do
  (box, st) <- runStateT (runReaderT ballGame initEnv) initGameState
  putStrLn box
  callGame initEnv st





mainTest' = do
  newVar <- newEmptyMVar

  forkIO
    (do
      a <- getChar
      putMVar newVar a
    )
  waiting' newVar

waiting' c = do
  a <- tryTakeMVar c
  case a of
    Just a -> putStrLn ("value inserted : " ++ [a] ++ "\n") >> mainTest'
    Nothing ->
      threadDelay 1000000 >> putStrLn "waiting for input" >> waiting' c



