module Main where

import           Test.Hspec
import           SameGameModels
import           Games
import           SearchState
import           Data.Maybe
import           List.Extra
import           Test.QuickCheck                ( property )


main :: IO ()
main = hspec $ do
  describe "sublists" $ do
    let prop_sublists_contain_same_elements :: Int -> [Int] -> Bool
        prop_sublists_contain_same_elements n xs =
          (n <= 0 && null (n `sublists` xs)) || mconcat (n `sublists` xs) == xs

        prop_result_contains_n_sublists :: Int -> [Int] -> Bool
        prop_result_contains_n_sublists n xs =
          n
            <= 0
            || (n > length xs && length (n `sublists` xs) == length xs)
            || length (n `sublists` xs)
            == n
    it "sublists contain same elements"
      $ property prop_sublists_contain_same_elements

    it "result contains n sublists" $ property prop_result_contains_n_sublists

  describe "mkSearchState" $ do
    it "should have empty result if result is nothing" $ do
      let st = mkSt [Position 2 2] Nothing
      bestResult st `shouldBe` Nothing

    it "should have empty result if path is not part of result (1)" $ do
      let result = resultFromPos [Position 0 0, Position 1 1, Position 2 2]
      let st     = mkSt [Position 0 0] result
      bestResult st `shouldSatisfy` isNothing

    it "should have empty result if path is not part of result (2)" $ do
      let result = resultFromPos [Position 0 0, Position 1 1, Position 2 2]
      let st     = mkSt [Position 9 9, Position 2 2] result
      bestResult st `shouldSatisfy` isNothing

    it "should have non empty result if path is part of result (1)" $ do
      let result = resultFromPos [Position 0 0, Position 1 1, Position 2 2]
      let st     = mkSt [] result
      bestResult st `shouldBe` result

    it "should have non empty result if path is part of result (2)" $ do
      let result = resultFromPos [Position 0 0, Position 1 1, Position 2 2]
      let st     = mkSt [Position 2 2] result
      bestResult st `shouldBe` result

    it "should have best result if move is part of it" $ do
      let result      = resultFromPos [Position 3 0]
      let searchState = mkSt [] result
      let nextState   = applyMove (Position 3 0) searchState
      bestResult nextState `shouldBe` result

    it "should not have best result if move is not part of it" $ do
      let result      = resultFromPos [Position 3 0]
      let searchState = mkSt [] result
      let nextState   = applyMove (Position 4 0) searchState
      bestResult nextState `shouldBe` Nothing

  describe "update" $ do
    it "should keep best result if move is part of it" $ do
      let result = Result
            (fromTuples [(0, 1), (1, 0), (2, 0), (1, 0), (0, 2), (1, 1), (4, 0)]
            )
            (Score 23)
      let searchState = mkSt [] (Just result)
      let nextState   = update (Position 4 0) result searchState
      bestResult nextState `shouldBe` Just result


    it "should update best result if current best result is empty" $ do
      let result = Result
            (fromTuples [(0, 1), (1, 0), (2, 0), (1, 0), (3, 0), (0, 2), (1, 1)]
            )
            (Score 23)
      let searchState = mkSt [] Nothing
      let nextState   = update (Position 1 1) result searchState
      bestResult nextState `shouldBe` Just result

    it "should update best result if new result is better" $ do
      let result = Result
            (fromTuples [(0, 1), (1, 0), (2, 0), (1, 0), (3, 0), (0, 2), (1, 1)]
            )
            (Score 23)
      let currentBest = Result
            (fromTuples [(0, 1), (1, 0), (2, 0), (1, 0), (0, 2), (1, 1), (4, 0)]
            )
            (Score 22)
      let searchState = mkSt [] (Just currentBest)
      let nextState   = update (Position 1 1) result searchState
      bestResult nextState `shouldBe` Just result

    it "should choose move from current best if current best is better" $ do
      let result = Result
            (fromTuples [(0, 1), (1, 0), (2, 0), (1, 0), (3, 0), (0, 2), (1, 1)]
            )
            (Score 22)
      let currentBest = Result
            (fromTuples [(0, 1), (1, 0), (2, 0), (1, 0), (0, 2), (1, 1), (4, 0)]
            )
            (Score 23)
      let searchState = mkSt [] (Just currentBest)
      let nextState   = update (Position 1 1) result searchState
      bestResult nextState `shouldBe` Just currentBest
      moves nextState `shouldBe` [Position 4 0]

resultFromPos :: [Position] -> Maybe Result
resultFromPos pos = Just $ Result pos (Score 0)

mkSt :: [Position] -> Maybe Result -> SearchState
mkSt = mkSearchState (mkGame 5)

fromTuples :: [(Int, Int)] -> [Position]
fromTuples = map (uncurry Position)
