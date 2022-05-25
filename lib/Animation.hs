module Animation
  ( Env(..)
  , GameState(..)
  , UserInput(..)
  , GameStatus(..)
  , initGameEnv
  , initGameState
  , updateState
  , render
  , parseUserInput
  , checkGameStatus
  ) where

import           Animation.Env
import           Animation.Render
import           Animation.State
import           Animation.Type
