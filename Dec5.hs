module Dec5 (solve, taskA, taskB) where
import Common
import Data.Char (toUpper)
import Data.List (sort)
import Data.Maybe (fromJust)

-- | Zipper-like thingy with two elements in focus.
data Zipper2 a = Z [a] [a]
  deriving Show

right :: Zipper2 a -> Maybe (Zipper2 a)
right (Z ls (r:rs@(_:_))) = Just $ Z (r:ls) rs
right z                   = Nothing

fromList :: [a] -> Zipper2 a
fromList (x:xs) = Z [x] xs

toList :: Zipper2 a -> [a]
toList (Z ls rs) = reverse ls ++ rs

focus :: Show a => Zipper2 a -> (a, a)
focus (Z (l:_) (r:_)) = (l, r)

dropFocus :: Zipper2 a -> Zipper2 a
dropFocus (Z (_:[]) (_:r:rs)) = Z [r] rs
dropFocus (Z (_:l:ls) (_:[])) = Z ls [l]
dropFocus (Z (_:ls) (_:rs))   = Z ls rs

reacts :: Char -> Char -> Bool
reacts a b = a /= b && toUpper a == toUpper b

react :: Zipper2 Char -> String
react z
  | uncurry reacts (focus z) = react (dropFocus z)
  | Just z' <- right z       = react z'
  | otherwise                = toList z

taskA :: String -> Int
taskA = length . react . fromList . concat . lines

taskB :: String -> Int
taskB i = head $ sort $ map (\t -> taskA $ filter (not . ofType t) i) ['a'..'z']
  where ofType t = flip elem [t, toUpper t]
