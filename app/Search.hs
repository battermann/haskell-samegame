module Main where

import           Games
import           MonteCarlo
import           SearchState
import           System.Environment
import           Control.Concurrent.ParallelIO.Local
import           GHC.Conc                       ( getNumCapabilities )

main :: IO ()
main = do
  args <- getArgs
  let (level, size) =
        case (\arg -> readMaybe arg :: Maybe Int) `traverse` args of
          Just (l : s : _) -> (l, s)
          _                -> (1, 15)
  print $ "level: " <> show level
  print $ "size:  " <> show size
  let searchState = fromGame (mkGame size)
  numCapabilities <- getNumCapabilities
  print $ "number of threads: " <> show numCapabilities
  _ <- withPool numCapabilities
    $ \pool -> mcts numCapabilities pool level searchState
  return ()


readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
  [(val, "")] -> Just val
  _           -> Nothing