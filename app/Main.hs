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
  wait threadVar  = do
    lift $ lift $ threadDelay (1000000 `div` 3)
    mVar <- lift $ lift $ tryTakeMVar threadVar
    case mVar of
      Just var -> case var of
        'a' -> do
          updateState (-1) -- Update Game State
          render -- Render Game
          st <- lift get
          if gameStatus st then
            mainGame
            else lift $ lift $ putStrLn "Game Over!!"
        'd' -> do
          updateState (1) -- Update Game State 
          render -- Render Game
          st <- lift get
          if gameStatus st then
            mainGame
            else lift $ lift $ putStrLn "Game Over!!"
        'q' -> do
          lift $lift $ putStrLn "End Game"
        _ -> do
          render -- Render Game
          st <- lift get
          if gameStatus st then
            mainGame
            else lift $ lift $ putStrLn "Game Over!!"
      Nothing -> do
        updateState (0) -- Update Game State
        render -- Render Game
        --render' -- Render Game
        st <- lift get
        if gameStatus st then
          wait threadVar
          else lift $lift $ putStrLn "Game Over!!"

main = do
  hSetEcho stdin False
  hSetBuffering stdin NoBuffering
  iGameState <- initGameSate
  runStateT (runReaderT mainGame initGameEnv) iGameState

