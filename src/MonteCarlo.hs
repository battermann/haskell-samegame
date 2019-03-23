{-# LANGUAGE TupleSections #-}
module MonteCarlo where

import           Control.Monad
import           Data.Foldable
import           Data.List
import qualified Data.Map                      as Map
import           Data.Map.Append
import           Data.Maybe
import           Data.Ord
import           List.Extra
import           SameGame
import           SameGameModels
import           System.Random
import           SearchState
import           Control.Concurrent.ParallelIO.Local

legalMoves :: Game -> [Position]
legalMoves (Finished _ _) = []
legalMoves (InProgress board _) =
  let positions = concatMap
        (\(ci, col) -> indexedMap (\ri _ -> Position ci ri) col)
        (zipWithIndex board)
      groups = nub $ mapMaybe (findGroup board) positions
  in  mapMaybe (\(Group _ ps) -> find (const True) ps) groups

newtype Add = Add { unwrap :: Int }

instance Semigroup Add where
  i1 <> i2 = Add $ unwrap i1 + unwrap i2

cellColor :: CellState -> AppendMap Color Add
cellColor (Filled color) = AppendMap $ Map.singleton color (Add 1)
cellColor Empty          = AppendMap $ Map.empty

predominantColor :: Game -> Maybe Color
predominantColor g =
  let colorOccurences =
          Map.toList $ unAppendMap $ (foldMap (foldMap cellColor) . getBoard) g
  in  case colorOccurences of
        []     -> Nothing
        colors -> Just $ fst (maximumBy (comparing (unwrap . snd)) colors)

colorAt :: Game -> Position -> Maybe Color
colorAt g pos = case cellStateAt (getBoard g) pos of
  Filled color -> Just color
  Empty        -> Nothing

tabuColorSimulationIO :: SearchState -> IO Result
tabuColorSimulationIO gs =
  let g         = game gs
      lMoves    = legalMoves g
      tabuColor = predominantColor g
  in  case partition (\p -> tabuColor == colorAt g p) lMoves of
        ([]       , []) -> return $ Result (moves gs) (getScore g)
        (tabuMoves, []) -> do
          n <- randomRIO (0, length tabuMoves - 1 :: Int)
          tabuColorSimulationIO (applyMove (tabuMoves !! n) gs)
        (_, nonTabuMoves) -> do
          n <- randomRIO (0, length nonTabuMoves - 1 :: Int)
          tabuColorSimulationIO (applyMove (nonTabuMoves !! n) gs)

runInParallel :: Int -> Pool -> [IO a] -> IO [a]
runInParallel parallelism pool ios =
  fmap mconcat $ sequence $ parallel pool <$> sublists parallelism ios

searchIO :: Int -> Pool -> Int -> Int -> SearchState -> IO SearchState
searchIO parallelism pool numLevels level searchState =
  let lMoves    = legalMoves (game searchState)
      resultsIO = if level <= 1
        then
          (\m -> (, m) <$> tabuColorSimulationIO (applyMove m searchState))
            <$> lMoves
        else
          (\m -> (\st -> (Result (moves st) (stScore st), m)) <$> searchIO
              parallelism
              pool
              numLevels
              (level - 1)
              (applyMove m searchState)
            )
            <$> lMoves
  in  do
        when (numLevels == level) $ print searchState
        results <- if numLevels == level
          then parallel pool resultsIO
          else sequence resultsIO
        case results of
          [] -> return searchState
          xs -> do
            let (result, m) = maximumBy (comparing (score . fst)) xs
            searchIO parallelism pool numLevels level
              $ update m result searchState

mcts :: Int -> Pool -> Int -> SearchState -> IO SearchState
mcts parallelism pool levels = searchIO parallelism pool levels levels
