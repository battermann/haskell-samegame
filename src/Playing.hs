module Playing
  ( playSameGame
  )
where

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
helpMsg =
  "\nUsage:\n\
          \\n\
          \  quit                          Exit the program\n\
          \  play <columnIndex> <rowIndex> Remove group at specified position"

playSameGame :: Game -> IO ()
playSameGame game = do
  putStrLn $ "\n" ++ show game
  putStr "> "
  input <- getLine
  case parseInput input of
    Just (Play position) -> playSameGame (play position game)
    Just Quit            -> return ()
    Nothing ->
      putStrLn "Invalid input" *> putStrLn helpMsg *> playSameGame game

