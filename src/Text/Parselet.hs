{-# LANGUAGE OverloadedStrings, LambdaCase, ViewPatterns, PatternSynonyms #-}
module Text.Parselet where

import Prelude hiding (drop, length, any, repeat, not)

import Control.Monad (void)
import Data.Text hiding (elem, any)

pattern a :< as <- (uncons -> Just (a, as))
pattern TEmpty <- (uncons -> Nothing)

newtype Parser a = Parser
  { parser :: Text -> Either Text (a, Text)
  }

runParser :: Text -> Parser a -> Either Text (a, Text)
runParser s p = parser p s

instance Functor Parser where
  fmap f p =
    Parser $ \s ->
      case runParser s p of
        Right (result, rest) -> Right (f result, rest)
        Left l -> Left l

instance Applicative Parser where
  pure x = Parser $ \s -> Right (x, s)
  pf <*> pa =
    Parser $ \s ->
      case runParser s pf of
        Left l -> Left l
        Right (f, rest) ->
          case runParser rest pa of
            Left l -> Left l
            Right (x, rest') -> Right (f x, rest')

instance Monad Parser where
  return = pure
  pa >>= f =
    Parser $ \s ->
      case runParser s pa of
        Left l -> Left l
        Right (result, rest) -> runParser rest (f result)

assocr :: Parser a -> Parser (a -> a -> a) -> Parser a
assocr var op = do
      v <- var
      rest v var op <|> pure v
  where
    rest x var op =
      (do
         f <- op
         f x <$> assocr var op)
        <|> pure x
  
nop :: Parser ()
nop = Parser $ \s -> Right ((), s)

fail :: Parser a
fail = Parser Left

phrase :: Text -> Parser Text
phrase match = Parser $ \s ->
  if match `isPrefixOf` s
  then Right (match, drop (length match) s)
  else Left s

phrase_ :: Text -> Parser ()
phrase_ t = void (phrase t)

upto :: Char ->  Parser ()
upto c = Parser $ \case
  ss@(s :< _) | s == c -> Right ((), ss)
  (_ :< rest) | otherwise -> runParser rest (upto c)
  s -> Left s

one :: Char -> Parser Char
one c = Parser $ \case
  (s :< rest) | s == c -> Right (s, rest)
  s -> Left s

one_ :: Char -> Parser ()
one_ c = void (one c)

any :: Parser Char
any =
  Parser $ \case
    (x :< xs) -> Right (x, xs)
    TEmpty -> Left ""

oneOf :: [Char] -> Parser Char
oneOf cs = Parser $ \case
  (s :< rest) | s `elem` cs -> Right (s, rest)
  s -> Left s

alphanumeric :: Parser Char
alphanumeric = oneOf "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890"

not :: Char -> Parser Char
not c =
  Parser $ \s@(x :< xs) ->
    if x /= c
      then Right (x, xs)
      else Left s

eof :: Parser Text
eof =
  Parser $ \case
    TEmpty -> Right ("", "")
    s -> Left s
  
repeat :: Parser Char -> Parser Text
repeat p = fmap pack (repeatUntil p)
  
word :: Parser Text
word = repeat alphanumeric

whitespace :: Parser ()
whitespace = optional $ void (repeat (oneOf " \n\t"))

(<|>) :: Parser a -> Parser a -> Parser a
(<|>) p q = Parser $ \s ->
  case runParser s p of
    Right (r, rest) -> Right (r, rest)
    _ -> runParser s q

optional :: Parser () -> Parser ()
optional p = p <|> nop

repeatUntil :: Parser a -> Parser [a]
repeatUntil p = go
  where
    go = do
      c <- p
      s <- go'
      pure (c : s)
    go' =
      (do
         c <- p
         s <- go'
         pure (c : s))
        <|> pure []

takeUntil :: Text -> Parser Text
takeUntil str =
  Parser $ \s ->
    case go str s of
      (TEmpty, TEmpty) -> Left s
      result -> Right result
  where
    go :: Text -> Text -> (Text, Text)
    go s (ss :< str2)
      | s `isPrefixOf` str2 = (singleton ss, str2)
      | otherwise =
        let (result, rest) = go s str2
         in (cons ss result, rest)
    go _ _ = (empty, empty)

toNext :: Text -> Parser Text
toNext t = phrase t <|> (any >> toNext t)
