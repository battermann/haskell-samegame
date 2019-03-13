module Main where

import           Games
import           MonteCarlo
import           SameGameModels

main :: IO ()
main = do
  _ <- nestedSearch 1 1 $ GameState [] (Score 0) defaultGame
  return ()
