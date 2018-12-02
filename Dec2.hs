module Dec2 (solve, taskA, taskB) where
import Common
import Data.List

hasN :: (Ord a, Eq a) => Int -> [a] -> Bool
hasN n = any ((== n) . length) . group . sort

taskA :: String -> Int
taskA inp = length (filter (hasN 2) lns) * length (filter (hasN 3) lns)
  where lns = lines inp

diffIndices :: Eq a => [a] -> [a] -> [Int]
diffIndices as bs = [i | (i, a, b) <- zip3 [0..] as bs, a /= b]

taskB :: String -> String
taskB inp = head $ do
    as <- lns
    bs <- lns
    case diffIndices as bs of
      [i] -> [deleteAt i as]
      _   -> []
  where
    lns = lines inp
    deleteAt 0 (x:xs) = xs
    deleteAt n (x:xs) = x : deleteAt (n-1) xs
    deleteAt _ _      = []
