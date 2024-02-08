{-# LANGUAGE OverloadedStrings, LambdaCase, ViewPatterns, PatternSynonyms #-}
module Text.Parselet where

import Prelude hiding (drop, length, any, repeat, not)

import Control.Monad (void)
import Data.Text hiding (elem, any)

pattern a :< as <- (uncons -> Just (a, as))
pattern TEmpty <- (uncons -> Nothing)

newtype Parser a = Parser
  { parser :: Text -> Maybe (a, Text)
  }

runParser :: Text -> Parser a -> Maybe (a, Text)
runParser s p = parser p s

instance Functor Parser where
  fmap f p =
    Parser $ \s ->
      case runParser s p of
        Nothing -> Nothing
        Just (result, rest) -> Just (f result, rest)

instance Applicative Parser where
  pure x = Parser $ \s -> Just (x, s)
  pf <*> pa =
    Parser $ \s ->
      case runParser s pf of
        Nothing -> Nothing
        Just (f, rest) ->
          case runParser rest pa of
            Nothing -> Nothing
            Just (x, rest') -> Just (f x, rest')

instance Monad Parser where
  return = pure
  pa >>= f =
    Parser $ \s ->
      case runParser s pa of
        Nothing -> Nothing
        Just (result, rest) -> runParser rest (f result)

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
nop = Parser $ \s -> Just ((), s)

fail :: Parser a
fail = Parser $ const Nothing

phrase :: Text -> Parser Text
phrase match = Parser $ \s ->
  if match `isPrefixOf` s
  then Just (match, drop (length match) s)
  else Nothing

phrase_ :: Text -> Parser ()
phrase_ t = void (phrase t)

upto :: Char ->  Parser ()
upto c = Parser $ \case
  ss@(s :< _) | s == c -> Just ((), ss)
  (_ :< rest) | otherwise -> runParser rest (upto c)
  _ -> Nothing

one :: Char -> Parser Char
one c = Parser $ \case
  (s :< rest) | s == c -> Just (s, rest)
  _ -> Nothing

one_ :: Char -> Parser ()
one_ c = void (one c)

any :: Parser Char
any =
  Parser $ \case
    (x :< xs) -> Just (x, xs)
    TEmpty -> Nothing

oneOf :: [Char] -> Parser Char
oneOf cs = Parser $ \case
  (s :< rest) | s `elem` cs -> Just (s, rest)
  _ -> Nothing

alphanumeric :: Parser Char
alphanumeric = oneOf "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890"

not :: Char -> Parser Char
not c =
  Parser $ \(x :< xs) ->
    if x /= c
      then Just (x, xs)
      else Nothing

eof :: Parser Text
eof =
  Parser $ \case
    TEmpty -> Just ("", "")
    _ -> Nothing
  
repeat :: Parser Char -> Parser Text
repeat p = fmap pack (repeatUntil p)
  
word :: Parser Text
word = repeat alphanumeric

whitespace :: Parser ()
whitespace = optional $ void (repeat (oneOf " \n\t"))

(<|>) :: Parser a -> Parser a -> Parser a
(<|>) p q = Parser $ \s ->
  case runParser s p of
    Just (r, rest) -> Just (r, rest)
    Nothing -> runParser s q

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
      (TEmpty, TEmpty) -> Nothing
      result -> Just result
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
