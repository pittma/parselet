{-# LANGUAGE LambdaCase, ViewPatterns, PatternSynonyms #-}
module Text.Parselet where

import Prelude hiding (drop, length)

import Control.Monad (void)
import Data.Text

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

nop :: Parser ()
nop = Parser $ \s -> Just ((), s)

phrase :: Text -> Parser a -> Parser a
phrase match p = Parser $ \s ->
  if match `isPrefixOf` s
  then runParser (drop (length match) s) p
  else Nothing

phrase_ :: Text -> Parser ()
phrase_ match = phrase match nop

eatTo :: Parser a -> Parser a
eatTo p =
  Parser $ \case
    ss@(_ :< rest) ->
      case parser p ss of
        Nothing -> parser (eatTo p) rest
        x -> x
    _ -> Nothing

upto :: Char ->  Parser ()
upto c = Parser $ \case
  ss@(s :< rest) | s == c -> Just ((), ss)
  ss@(s :< rest) | otherwise -> runParser rest (upto c)
  _ -> Nothing

toNext :: Text -> Parser ()
toNext  s = eatTo (phrase s nop)

one :: Char -> Parser Char
one c = Parser $ \case
  (s :< rest) | s == c -> Just (s, rest)
  (s :< rest) | otherwise -> Nothing
  _ -> Nothing

one_ :: Char -> Parser ()
one_ c = void (one c)

many :: Char -> Parser Text
many c = Parser $ \s ->
  let res = go c s
  in Just (res, drop (length res) s)
  where
    go :: Char -> Text -> Text
    go _ TEmpty = empty
    go c (s :< rest) =
      if s == c
        then cons s (go c rest)
        else empty

(<|>) :: Parser a -> Parser a -> Parser a
(<|>) p q = Parser $ \s ->
  case runParser s p of
    Just (r, rest) -> Just (r, rest)
    Nothing -> runParser s q

optional :: Parser () -> Parser ()
optional p = p <|> nop

repeatUntil :: Parser a -> Parser [a]
repeatUntil p = Parser $ \s -> Just $ go p s
  where
    go :: Parser a -> Text -> ([a], Text)
    go p s =
      case runParser s p of
        Just (res, rest) ->
          let (r, rr) = go p rest
           in (res : r, rr)
        Nothing -> ([], s)

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
    go _ TEmpty = (empty, empty)
