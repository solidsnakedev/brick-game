module Animation.Env (Env(..), initGameEnv) where

data Env = Env
  { column    :: Int
  , row       :: Int
  , boardSize :: Int
  }
  deriving Show

initGameEnv = Env 10 10 3