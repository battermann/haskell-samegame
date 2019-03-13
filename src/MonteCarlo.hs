module MonteCarlo where

import           SameGameModels
import           SameGame
import           List.Extra
import           Data.Maybe
import           Data.Foldable
import           Data.List
import           System.Random
import           Control.Monad

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

compScore :: (a, Score) -> (a, Score) -> Ordering
compScore (_, Score sc1) (_, Score sc2) | sc1 > sc2 = GT
                                        | sc1 < sc2 = LT
                                        | otherwise = EQ

nestedSearch :: Int -> Int -> GameState -> IO GameState
nestedSearch numLevels level gameState =
  when (numLevels == level) (print gameState)
    *> case legalMoves (position gameState) of
         [] -> return gameState
         moves ->
           let simulationResults = traverse
                 (\move -> if level == 1
                   then (\(Result _ sc) -> (move, sc))
                     <$> simulation (applyMove move gameState)
                   else (\gs -> (move, score gs)) <$> nestedSearch
                     numLevels
                     (level - 1)
                     (applyMove move gameState)
                 )
                 moves
               best = fst . maximumBy compScore <$> simulationResults
           in  best >>= nestedSearch numLevels level . (`applyMove` gameState)
