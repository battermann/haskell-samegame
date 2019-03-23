module List.Extra
  ( zipWithIndex
  , indexedMap
  , elemAt
  , sublists
  )
where

import           Control.Lens                   ( element
                                                , (^?)
                                                )

zipWithIndex :: [a] -> [(Int, a)]
zipWithIndex = zip [0 ..]

indexedMap :: (Int -> a -> b) -> [a] -> [b]
indexedMap f xs = uncurry f <$> zipWithIndex xs

elemAt :: Int -> [a] -> Maybe a
elemAt index xs = xs ^? element index

sublists :: Int -> [a] -> [[a]]
sublists _ [] = []
sublists 1 xs = [xs]
sublists numSublists xs
  | numSublists <= 0
  = []
  | numSublists > length xs
  = (: []) <$> xs
  | otherwise
  = let n = length xs `div` numSublists
    in  take n xs : sublists (numSublists - 1) (drop n xs)
