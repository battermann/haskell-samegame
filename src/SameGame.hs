module SameGame
  ( play
  , evaluateGameState
  , findGroup
  )
where

import           Data.Maybe
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           SameGameModels
import           List.Extra

bonus :: Score
bonus = Score 1000

calcScore :: Group -> Score
calcScore (Group _ positions) =
  let x = (length positions - 2) in Score (x * x)

penalty :: Int -> Score
penalty numberOfStonesLeft =
  Score $ -((numberOfStonesLeft - 2) * (numberOfStonesLeft - 2))

cellStateAt :: Board -> Position -> CellState
cellStateAt board position = fromMaybe
  Empty
  (elemAt (columnIndex position) board >>= elemAt (rowIndex position))

left :: Position -> Position
left position = position { columnIndex = columnIndex position - 1 }

right :: Position -> Position
right position = position { columnIndex = columnIndex position + 1 }

up :: Position -> Position
up position = position { rowIndex = rowIndex position + 1 }

down :: Position -> Position
down position = position { rowIndex = rowIndex position - 1 }

hasColor :: Color -> Cell -> Bool
hasColor c (Cell _ (Filled cellColor)) = c == cellColor
hasColor _ (Cell _ Empty             ) = False

adjacentWithSameColor :: Board -> Position -> Set Position
adjacentWithSameColor board position =
  let adjacentCells = Set.fromList $ map
        (\p -> Cell p (cellStateAt board p))
        [up position, right position, down position, left position]
  in  case cellStateAt board position of
        Filled c -> Set.map getPosition $ Set.filter (hasColor c) adjacentCells
        Empty    -> Set.empty

allCells :: Board -> [Cell]
allCells columns = concatMap
  (\(col, cells) -> indexedMap (makeCell col) cells)
  (zipWithIndex columns)

hasAdjacentWithSameColor :: Board -> Cell -> Bool
hasAdjacentWithSameColor board cell =
  not . Set.null $ adjacentWithSameColor board (getPosition cell)

hasValidMoves :: Board -> Bool
hasValidMoves board = any (hasAdjacentWithSameColor board) (allCells board)

totalNumberOfCells :: Board -> Int
totalNumberOfCells =
  let countOne (Filled _) = 1
      countOne Empty      = 0
  in  sum . map (sum . map countOne)


columnIsEmpty :: Column -> Bool
columnIsEmpty []          = True
columnIsEmpty (Empty : _) = True
columnIsEmpty _           = False

isEmpty :: Board -> Bool
isEmpty [] = True
isEmpty xs = all columnIsEmpty xs

evaluateGameState :: Board -> Score -> Game
evaluateGameState board score = if hasValidMoves board
  then InProgress board score
  else if isEmpty board
    then Finished board (score `add` bonus)
    else Finished board $ score `add` (penalty . totalNumberOfCells) board

find :: Board -> Set Position -> Set Position -> Set Position
find board toSearch group = case Set.toList toSearch of
  []    -> group
  h : t -> find board stillToSearch cellsFoundSoFar
   where
    cellsWithSameColor = adjacentWithSameColor board h
    cellsFoundSoFar    = Set.insert h group
    stillToSearch      = Set.difference
      (Set.union cellsWithSameColor (Set.fromList t))
      cellsFoundSoFar

findGroup :: Board -> Position -> Maybe Group
findGroup board position = case cellStateAt board position of
  Filled color ->
    let touchingPositionsOfSameColor =
            find board (Set.singleton position) Set.empty
    in  if length touchingPositionsOfSameColor > 1
          then Just $ Group color touchingPositionsOfSameColor
          else Nothing

  Empty -> Nothing

removeGroup :: Group -> Board -> Board
removeGroup (Group _ positions) board =
  let fillWithEmpty n cols = cols ++ replicate (n - length cols) Empty

      fillWithEmptyColumns n m cols =
          cols ++ replicate (n - length cols) (replicate m Empty)

      columnHeight []      = 0
      columnHeight (h : _) = length h

      notInGroup (Cell pos _) = notElem pos positions

      removeCells c col = map
        getCellState
        ( filter notInGroup
        $ indexedMap (\r cell -> Cell (Position c r) cell) col
        )

      nonEmpty        = not . columnIsEmpty

      nonEmptyColumns = filter nonEmpty $ indexedMap
        (\i col -> fillWithEmpty (length col) (removeCells i col))
        board
  in  fillWithEmptyColumns (length board) (columnHeight board) nonEmptyColumns

play :: Position -> Game -> Game
play position (InProgress board score) = case findGroup board position of
  Just group ->
    evaluateGameState (removeGroup group board) (score `add` calcScore group)
  Nothing -> InProgress board score
play _ game = game
