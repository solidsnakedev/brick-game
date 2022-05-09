module Animation.Env (Env(..), initEnv) where

data Env = Env
  { column    :: Int
  , row       :: Int
  , boardSize :: Int
  }
  deriving Show

initEnv = Env 10 10 3