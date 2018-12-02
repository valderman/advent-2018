module Common (solve) where

solve :: Show a => (String -> a) -> FilePath -> IO ()
solve f file = readFile file >>= print . f
