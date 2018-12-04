{-# LANGUAGE LambdaCase #-}
module Common
  ( module Parser
  , solve
  , head2
  , soup, soupBy, greatestBy, greatestBy_
  ) where
import Data.List (group, sort, groupBy, sortBy)
import Data.Maybe (fromJust, listToMaybe)
import Parser

solve :: Show a => (String -> a) -> FilePath -> IO ()
solve f file = readFile file >>= print . f

head2 :: [[a]] -> a
head2 = head . head

soup :: (Eq a, Ord a) => [a] -> [[a]]
soup = group . sort

soupBy :: (a -> a -> Ordering) -> [a] -> [[a]]
soupBy f = groupBy (\a b -> f a b == EQ) . sortBy f

greatestBy :: (a -> a -> Ordering) -> [a] -> Maybe a
greatestBy f = listToMaybe . sortBy (flip f)

greatestBy_ :: (a -> a -> Ordering) -> [a] -> a
greatestBy_ f = fromJust . greatestBy f
