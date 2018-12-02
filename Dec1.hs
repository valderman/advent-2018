{-# LANGUAGE TypeApplications #-}
module Dec1 (solve, taskA, taskB) where
import Common
import Data.List (foldl')
import qualified Data.Set as Set

taskA :: String -> Int
taskA = sum . map (read . filter (/= '+')) . lines

taskB :: String -> Int
taskB = find2 Set.empty . scanl (+) 0 . preprocess
  where
    find2 s (x:xs)
      | Set.member x s = x
      | otherwise      = find2 (Set.insert x s) xs
    preprocess = cycle . map (read . filter (/= '+')) . lines
