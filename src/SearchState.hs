module SearchState
  ( Result(..)
  , mkSearchState
  , applyMove
  , SearchState
  , stScore
  , moves
  , game
  , bestResult
  , fromGame
  , update
  )
where

import           Data.List
import           SameGame
import           SameGameModels

data Result = Result
  { path  :: [Position]
  , score :: Score
  } deriving (Eq, Ord)

instance Show Result where
  show result =
    show (score result) ++ "\n     " ++ show (reverse $ path result)

data SearchState = SearchState
  { game       :: Game
  , moves      :: [Position]
  , bestResult :: Maybe Result
  }

instance Show SearchState where
  show searchState =
    show (game searchState) ++ "\n" ++ show ((reverse . moves) searchState)

stScore :: SearchState -> Score
stScore = getScore . game

mkSearchState :: Game -> [Position] -> Maybe Result -> SearchState
mkSearchState g positions Nothing       = SearchState g positions Nothing
mkSearchState g positions (Just result) = if positions `isSuffixOf` path result
  then SearchState g positions (Just result)
  else SearchState g positions Nothing

fromGame :: Game -> SearchState
fromGame g = mkSearchState g [] Nothing

applyMove :: Position -> SearchState -> SearchState
applyMove pos searchState = mkSearchState (play pos (game searchState))
                                          (pos : moves searchState)
                                          (bestResult searchState)

update :: Position -> Result -> SearchState -> SearchState
update pos result toUpdate =
  let nextState        = play pos (game toUpdate)
      playedMoves      = pos : moves toUpdate
      maybeCurrentBest = bestResult toUpdate
  in  case maybeCurrentBest of
        Nothing          -> SearchState nextState playedMoves (Just result)
        Just currentBest -> if score result >= score currentBest
          then SearchState nextState playedMoves (Just result)
          else
            let i = length (path currentBest) - 1 - length (moves toUpdate)
                m = path currentBest !! i
            in  SearchState (play m (game toUpdate))
                            (m : moves toUpdate)
                            (Just currentBest)
