module Main where
import           Animation                      ( Env(..)
                                                , GameState(..)
                                                , GameStatus(..)
                                                , UserInput(..)
                                                , checkGameStatus
                                                , initGameEnv
                                                , initGameState
                                                , parseUserInput
                                                , render
                                                , updateState
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
                                                , when
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

mainGame :: ReaderT Env (StateT GameState IO) ()
mainGame = do
  newVar <- lift $ lift newEmptyMVar

  lift $ lift $ forkIO
    (do
      a <- getChar
      putChar '\n'
      putMVar newVar a
    )

  wait newVar

 where
  wait threadVar = do
    lift $ lift $ threadDelay (1000000 `div` 6)
    mVar <- lift $ lift $ tryTakeMVar threadVar
    case parseUserInput mVar of
      MoveLeft -> do
        updateState (-1) -- Update Game State
        render -- Render Game
        checkGameStatus mainGame "Game Over!!"
      MoveRight -> do
        updateState 1 -- Update Game State 
        render -- Render Game
        checkGameStatus mainGame "Game Over!!"
      Quit -> do
        lift $ lift $ putStrLn "End Game"
      InvalidInput -> do
        render -- Render Game
        checkGameStatus mainGame "Game Over!!"
      NoInput -> do
        updateState 0 -- Update Game State
        render -- Render Game
        checkGameStatus (wait threadVar) "Game Over!!"

main = do
  hSetEcho stdin False
  hSetBuffering stdin NoBuffering
  iGameState <- initGameState
  runStateT (runReaderT mainGame initGameEnv) iGameState