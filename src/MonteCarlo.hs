module MonteCarlo where

import           SameGameModels
import           SameGame
import           List.Extra
import           Data.Maybe
import           Data.Foldable
import           Data.List
import           System.Random
import           Control.Monad
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map
import           Data.Ord

data GameState = GameState
  { playedMoves :: [Position]
  , score :: Score
  , position :: Game
  }

instance Show GameState where
  show gameState = show (position gameState) ++ "\n" ++ show
    ((reverse . playedMoves) gameState)

data Result = Result [Position] Score

legalMoves :: Game -> [Position]
legalMoves (Finished _ _) = []
legalMoves (InProgress board _) =
  let positions = concatMap
        (\(ci, col) -> indexedMap (\ri _ -> Position ci ri) col)
        (zipWithIndex board)
      groups = nub $ mapMaybe (findGroup board) positions
  in  mapMaybe (\(Group _ ps) -> find (const True) ps) groups

applyMove :: Position -> GameState -> GameState
applyMove p gs =
  let game' = play p (position gs)
  in  GameState (p : playedMoves gs) (getScore game') game'

simulation :: GameState -> IO Result
simulation gameState = case legalMoves (position gameState) of
  []    -> return $ Result (playedMoves gameState) (score gameState)
  moves -> do
    n <- randomRIO (0, length moves - 1 :: Int)
    simulation (applyMove (moves !! n) gameState)

cellColor :: CellState -> Map Color Int
cellColor (Filled color) = Map.singleton color 1
cellColor Empty          = Map.empty

predominantColor :: GameState -> Maybe Color
predominantColor gameState =
  let colorOccurences = Map.toList
        $ (foldMap (foldMap cellColor) . getBoard . position) gameState
  in  case colorOccurences of
        []     -> Nothing
        colors -> Just $ fst (maximumBy (comparing snd) colors)

colorAt :: Game -> Position -> Maybe Color
colorAt game pos = case cellStateAt (getBoard game) pos of
  Filled color -> Just color
  Empty        -> Nothing

tabuColorSimulation :: GameState -> IO Result
tabuColorSimulation gameState =
  let game      = position gameState
      moves     = legalMoves game
      tabuColor = predominantColor gameState
  in  case partition (\p -> tabuColor == colorAt game p) moves of
        ([], []) -> return $ Result (playedMoves gameState) (score gameState)
        (tabuMoves, []) -> do
          n <- randomRIO (0, length tabuMoves - 1 :: Int)
          tabuColorSimulation (applyMove (tabuMoves !! n) gameState)
        (_, nonTabuMoves) -> do
          n <- randomRIO (0, length nonTabuMoves - 1 :: Int)
          tabuColorSimulation (applyMove (nonTabuMoves !! n) gameState)

nestedSearch :: Int -> Int -> GameState -> IO GameState
nestedSearch numLevels level gameState =
  when (numLevels == level) (print gameState)
    *> case legalMoves (position gameState) of
         [] -> return gameState
         moves ->
           let simulationResults = traverse
                 (\move -> if level == 1
                   then (\(Result _ sc) -> (move, sc))
                     <$> tabuColorSimulation (applyMove move gameState)
                   else (\gs -> (move, score gs)) <$> nestedSearch
                     numLevels
                     (level - 1)
                     (applyMove move gameState)
                 )
                 moves
               best = fst . maximumBy (comparing snd) <$> simulationResults
           in  best >>= nestedSearch numLevels level . (`applyMove` gameState)
