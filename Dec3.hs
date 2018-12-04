{-# LANGUAGE RecordWildCards #-}
module Dec3 (solve, taskA, taskB) where
import Common
import Data.Function (on)
import Data.List ((\\), sortBy, groupBy)

data Claim = Claim
  { claimId :: Int
  , claimTopLeft :: (Int, Int)
  , claimSize :: (Int, Int)
  } deriving Show

data ClaimedSquare = Square
  { sqId :: Int
  , sqCoords :: (Int, Int)
  } deriving Show

claim :: Parser Char Claim
claim =
  Claim <$> (thisChar '#' *> int <* string " @ ")
        <*> (pair "," int int <* string ": ")
        <*> (pair "x" int int <* eof)

allCoords :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
allCoords (x0, y0) (w, h) =
  [ (x, y)
  | x <- [x0 .. x0+w-1]
  , y <- [y0 .. y0+h-1]
  ]

allClaims :: Claim -> [ClaimedSquare]
allClaims (Claim {..}) = map (Square claimId) $ allCoords claimTopLeft claimSize

parseClaims :: String -> [ClaimedSquare]
parseClaims = concat . map (allClaims . parse_ claim) . lines

overlapGroups :: [ClaimedSquare] -> [[ClaimedSquare]]
overlapGroups = groupBy ((==) `on` sqCoords) . sortBy (compare `on` sqCoords)

parseOverlapGroups :: String -> [[ClaimedSquare]]
parseOverlapGroups = overlapGroups . parseClaims

taskA :: String -> Int
taskA = length . filter ((>1) . length) . parseOverlapGroups

taskB :: String -> Int
taskB inp = head $ singles groups \\ shareds groups
  where
    groups = parseOverlapGroups inp
    -- IDs that have a monopoly on some square
    singles = map head . soup . map sqId . concat . filter ((==1) . length)
    -- IDs that share at least one square
    shareds = map head . soup . map sqId . concat . filter ((>1) . length)
