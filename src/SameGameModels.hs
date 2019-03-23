module SameGameModels where

import           Data.List
import           Data.Set                       ( Set )

data Position = Position
  { columnIndex :: Int
  , rowIndex :: Int
  } deriving (Eq, Ord)

newtype Color =
  Color Int deriving (Eq, Ord, Show)

data CellState
  = Empty
  | Filled Color deriving (Eq, Ord)

type Column = [CellState]

type Board = [Column]

data Cell = Cell Position CellState deriving (Eq, Ord)

data Group = Group Color (Set Position) deriving (Eq, Ord)

newtype Score = Score Int deriving (Eq, Ord, Show)

add :: Score -> Score -> Score
add (Score sc1) (Score sc2) = Score $ sc1 + sc2

data Game
  = InProgress Board Score
  | Finished Board Score

getBoard :: Game -> Board
getBoard (InProgress board _) = board
getBoard (Finished   board _) = board

getScore :: Game -> Score
getScore (InProgress _ score) = score
getScore (Finished   _ score) = score

getCellState :: Cell -> CellState
getCellState (Cell _ state) = state

getPosition :: Cell -> Position
getPosition (Cell position _) = position

makeCell :: Int -> Int -> CellState -> Cell
makeCell col row = Cell (Position col row)

instance Show CellState where
  show (Filled (Color color)) = show color
  show Empty                  = "-"

instance Show Game where
  show game =
    intercalate "\n"
                (unwords . map show <$> transpose (reverse <$> getBoard game))
      ++ "\n"
      ++ show (getScore game)

instance Show Position where
  show position =
    "(" ++ show (columnIndex position) ++ "," ++ show (rowIndex position) ++ ")"
