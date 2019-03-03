module Util.List
  ( zipWithIndex
  , indexedMap
  , elemAt
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
