{-# LANGUAGE LambdaCase #-}
module Parser
  ( Parser , parse, parse_
  , MonadPlus (..), Alternative (..)
  , thisChar, pair, anyChar, char, string, many1, digit, int, try, eof, msum
  ) where
import Control.Applicative
import Control.Monad
import Data.Char
newtype Parser t a = P { unP :: [t] -> Maybe ([t], a) }

instance Functor (Parser t) where
  fmap f (P x) = P $ \s -> do
    (s', x') <- x s
    Just (s', f x')

instance Applicative (Parser t) where
  pure x = P $ \s -> Just (s, x)
  P f <*> P x = P $ \s -> do
    (s', f') <- f s
    (s'', x') <- x s'
    Just (s'', f' x')

instance Alternative (Parser t) where
  empty = P $ const Nothing
  P a <|> P b = P $ \s ->
    case a s of
      result@(Just _) -> result
      _               -> b s

instance Monad (Parser t) where
  return = pure
  P m >>= f = P $ \s -> do
    (s', x) <- m s
    unP (f x) s'
  fail = const empty

instance MonadPlus (Parser t) where
  mzero = empty
  mplus = (<|>)

parse :: Parser t a -> [t] -> Maybe a
parse p s = snd <$> unP p s

parse_ :: Parser t a -> [t] -> a
parse_ p s = case parse p s of
  Just x -> x
  _      -> error "no parse"

anyChar :: Parser t t
anyChar = char (const True)

char :: (t -> Bool) -> Parser t t
char p = P $ \case
  (c:cs) | p c -> Just (cs, c)
  _            -> Nothing

string :: Eq t => [t] -> Parser t ()
string s = mapM_ thisChar s

pair :: Eq t => [t] -> Parser t a -> Parser t b -> Parser t (a, b)
pair delim pa pb = do
  a <- pa
  string delim
  b <- pb
  pure (a, b)

thisChar :: Eq t => t -> Parser t t
thisChar c = char (== c)

many1 :: Parser t a -> Parser t [a]
many1 p = do
  xs <- many p
  case xs of
    [] -> fail ":("
    _  -> pure xs

digit :: Parser Char Char
digit = char isDigit

int :: Parser Char Int
int = (char (== '-') >> fmap (negate) positiveInt) <|> positiveInt
  where
    positiveInt = read <$> many1 digit

try :: Parser t a -> Parser t (Maybe a)
try (P p) = P $ \s ->
  case p s of
    Just (s', x) -> Just (s', Just x)
    _            -> Just (s, Nothing)

eof :: Parser t ()
eof = P $ \case
  [] -> Just ([], ())
  _  -> Nothing
