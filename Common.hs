{-# LANGUAGE LambdaCase #-}
module Common (module Parser, solve) where
import Parser

solve :: Show a => (String -> a) -> FilePath -> IO ()
solve f file = readFile file >>= print . f
