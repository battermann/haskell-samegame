module Main where

import           Games
import           MonteCarlo
import           SearchState
import           System.Environment
import           GHC.Conc                       ( getNumCapabilities )
import           Data.IORef

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
  globalBest      <- newIORef (bestResult searchState)
  numCapabilities <- getNumCapabilities
  print $ "number of threads: " <> show numCapabilities
  _ <- mcts globalBest level searchState
  return ()


readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
  [(val, "")] -> Just val
  _           -> Nothing
