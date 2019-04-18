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
import           SearchState
import           Control.Concurrent.ParallelIO.Local
import           Data.IORef
import           Random.Xorshift.Int32

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
cellColor Empty          = AppendMap Map.empty

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

rnd :: Int -> IO Int
rnd n = do
  gen <- newXorshift32
  return $ (fromIntegral $ getInt32 gen :: Int) `mod` (n + 1)


tabuColorSimulationIO :: SearchState -> IO Result
tabuColorSimulationIO gs =
  let g         = game gs
      lMoves    = legalMoves g
      tabuColor = predominantColor g
  in  case partition (\p -> tabuColor == colorAt g p) lMoves of
        ([]       , []) -> pure $ Result (moves gs) (getScore g)
        (tabuMoves, []) -> do
          n <- rnd (length tabuMoves - 1)
          tabuColorSimulationIO (applyMove (tabuMoves !! n) gs)
        (_, nonTabuMoves) -> do
          n <- rnd (length nonTabuMoves - 1)
          tabuColorSimulationIO (applyMove (nonTabuMoves !! n) gs)

updateGlobalBest :: IORef (Maybe Result) -> Maybe Result -> IO ()
updateGlobalBest globalBest maybeResult = do
  modifyResult <- atomicModifyIORef
    globalBest
    (\maybeGlBest -> case (maybeGlBest, maybeResult) of
      (Nothing    , result     ) -> (result, result)
      (Just glBest, Just result) -> if score glBest >= score result
        then (Just glBest, Nothing)
        else (Just result, Just result)
      (Just glBest, Nothing) -> (Just glBest, Nothing)
    )
  case modifyResult of
    Nothing -> pure ()
    Just better ->
      putStrLn $ ">>> improved sequence found:\n" <> show better <> "\n"

searchIO
  :: Int
  -> Pool
  -> IORef (Maybe Result)
  -> Int
  -> Int
  -> SearchState
  -> IO SearchState
searchIO parallelism pool globalBest numLevels level searchState =
  let
    lMoves    = legalMoves (game searchState)
    resultsIO = if level <= 1
      then
        (\m -> (, m) <$> tabuColorSimulationIO (applyMove m searchState))
          <$> lMoves
      else
        (\m -> (\st -> (Result (moves st) (stScore st), m)) <$> searchIO
            parallelism
            pool
            globalBest
            numLevels
            (level - 1)
            (applyMove m searchState)
          )
          <$> lMoves
  in
    do
      when (numLevels == level) $ print searchState
      results <- if numLevels == level
        then parallelInterleaved pool resultsIO
        else sequence resultsIO
      case results of
        [] -> pure searchState
        xs -> do
          let (result, m)     = maximumBy (comparing (score . fst)) xs
          let nextSearchState = update m result searchState
          updateGlobalBest globalBest (bestResult nextSearchState)
          searchIO parallelism pool globalBest numLevels level nextSearchState

mcts
  :: Int -> Pool -> IORef (Maybe Result) -> Int -> SearchState -> IO SearchState
mcts parallelism pool globalBest levels =
  searchIO parallelism pool globalBest levels levels
