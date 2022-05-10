module Main where
import           Animation                      ( Env(..)
                                                , GameState(..)
                                                , initGameEnv
                                                , initGameSate
                                                , updateState
                                                , render
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

mainGame :: ReaderT Env (StateT GameState IO) String
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
  wait threadVar  = do
    lift $ lift $ threadDelay (1000000 `div` 3)
    mVar <- lift $ lift $ tryTakeMVar threadVar
    case mVar of
      Just var -> case var of
        'a' -> do
          updateState (-1) -- Update Game State
          render -- Render Game
          mainGame -- Return to Game
        'd' -> do
          updateState (1) -- Update Game State 
          render -- Render Game
          mainGame -- Return to Game
        'q' -> do
          return ""
        _ -> do
          render -- Render Game
          mainGame
      Nothing -> do
        updateState (0) -- Update Game State
        render -- Render Game
        st <- lift get
        if gameStatus st then
          wait threadVar
          else return ""

main = do
  hSetEcho stdin False
  hSetBuffering stdin NoBuffering
  runStateT (runReaderT mainGame initGameEnv) initGameSate

