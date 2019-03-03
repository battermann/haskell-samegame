module Main where

import           SameGameModels
import           SameGame
import           Data.Char
import           Text.Read

data Command = Quit | Play Position

parseInput :: String -> Maybe Command
parseInput input = case (words . map toLower) input of
  ["quit"]                 -> Just Quit
  ["play", colStr, rowStr] -> do
    col <- readMaybe colStr
    row <- readMaybe rowStr
    return $ Play $ Position col row
  _ -> Nothing

helpMsg :: String 
helpMsg = "\nusage:\n\
          \\n\
          \  quit                          exit the program\n\
          \  play <columnIndex> <rowIndex> remove group at specified position"

playSameGame :: Game -> IO ()
playSameGame game = do
  putStrLn $ "\n" ++ show game
  putStr "> "
  input <- getLine
  case parseInput input of
    Just (Play position) -> playSameGame (play position game)
    Just Quit            -> return ()
    Nothing              -> putStrLn helpMsg *> playSameGame game

main :: IO ()
main = playSameGame defaultGame

defaultGame :: Game
defaultGame = evaluateGameState columns (Score 0)
 where
  columns = map
    (map (Filled . Color))
    [ [1, 1, 3, 2, 0, 0, 1, 1, 2, 0, 1, 2, 0, 3, 3]
    , [0, 0, 3, 2, 4, 3, 3, 0, 4, 4, 2, 3, 2, 3, 1]
    , [1, 3, 0, 3, 1, 3, 3, 0, 3, 4, 1, 4, 3, 2, 1]
    , [0, 2, 2, 1, 2, 4, 2, 4, 4, 3, 3, 0, 4, 0, 4]
    , [1, 1, 3, 0, 0, 2, 0, 0, 2, 0, 1, 2, 3, 4, 1]
    , [1, 4, 2, 4, 1, 3, 4, 3, 3, 3, 2, 3, 0, 4, 0]
    , [2, 4, 1, 0, 3, 0, 3, 1, 1, 4, 0, 0, 3, 1, 4]
    , [2, 4, 4, 1, 4, 0, 1, 2, 1, 2, 1, 2, 0, 3, 0]
    , [1, 4, 3, 2, 3, 2, 3, 1, 1, 2, 2, 4, 0, 1, 4]
    , [0, 0, 1, 4, 3, 1, 0, 0, 3, 2, 1, 4, 3, 2, 4]
    , [0, 4, 3, 1, 4, 2, 4, 4, 4, 0, 0, 4, 4, 0, 1]
    , [1, 2, 0, 3, 1, 3, 1, 1, 1, 2, 3, 3, 4, 0, 1]
    , [4, 1, 2, 3, 4, 4, 0, 3, 0, 3, 4, 0, 1, 4, 0]
    , [3, 3, 1, 0, 0, 0, 0, 3, 3, 4, 0, 2, 1, 0, 2]
    , [2, 4, 3, 1, 4, 1, 3, 1, 1, 0, 1, 3, 1, 4, 3]
    ]
