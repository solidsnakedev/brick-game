module Main where
import           Animation                      ( DirectionX(..)
                                                , DirectionY(..)
                                                , Env(..)
                                                , GameState(..)
                                                , createBoard
                                                , createBox
                                                , initEnv
                                                , initGameSate
                                                , cleanScreen
                                                , animationHelper
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
          -- Update Game State
          animationHelper (-1)
          -- Render Game
          render
          -- Return to Game
          mainGame
        'd' -> do
          -- Update Game State          
          animationHelper (1)
          -- Render Game
          render
          -- Return to Game
          mainGame
        'q' -> do
          return ""
        _ -> do
          -- Render Game
          render
          --lift $ lift $ threadDelay 100000
          mainGame
      Nothing -> do
        -- Update Game State
        animationHelper (0)
        -- Render Game
        render

        wait threadVar

main = do
  hSetEcho stdin False
  hSetBuffering stdin NoBuffering
  runStateT (runReaderT mainGame initEnv) initGameSate

