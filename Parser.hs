{-# LANGUAGE LambdaCase #-}
module Parser
  ( Parser , parse
  , Alternative (..), thisChar, pair, char, string, many1, digit, int, try, eof
  ) where
import Control.Applicative
import Data.Char
newtype Parser a = P { unP :: String -> Maybe (String, a) }

instance Functor Parser where
  fmap f (P x) = P $ \s -> do
    (s', x') <- x s
    Just (s', f x')

instance Applicative Parser where
  pure x = P $ \s -> Just (s, x)
  P f <*> P x = P $ \s -> do
    (s', f') <- f s
    (s'', x') <- x s'
    Just (s'', f' x')

instance Alternative Parser where
  empty = P $ const Nothing
  P a <|> P b = P $ \s ->
    case a s of
      result@(Just _) -> result
      _               -> b s

instance Monad Parser where
  return = pure
  P m >>= f = P $ \s -> do
    (s', x) <- m s
    unP (f x) s'
  fail = const empty

parse :: Parser a -> String -> Maybe a
parse p s = snd <$> unP p s

char :: (Char -> Bool) -> Parser Char
char p = P $ \case
  (c:cs) | p c -> Just (cs, c)
  _            -> Nothing

string :: String -> Parser ()
string s = mapM_ thisChar s

pair :: String -> Parser a -> Parser b -> Parser (a, b)
pair delim pa pb = do
  a <- pa
  string delim
  b <- pb
  pure (a, b)

thisChar :: Char -> Parser Char
thisChar c = char (== c)

many1 :: Parser a -> Parser [a]
many1 p = do
  xs <- many p
  case xs of
    [] -> fail ":("
    _  -> pure xs

digit :: Parser Char
digit = char isDigit

int :: Parser Int
int = (char (== '-') >> fmap (negate) positiveInt) <|> positiveInt
  where
    positiveInt = read <$> many1 digit

try :: Parser a -> Parser (Maybe a)
try (P p) = P $ \s ->
  case p s of
    Just (s', x) -> Just (s', Just x)
    _            -> Just (s, Nothing)

eof :: Parser ()
eof = P $ \case
  [] -> Just ([], ())
  _  -> Nothing
