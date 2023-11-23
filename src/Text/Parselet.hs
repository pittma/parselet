{-# LANGUAGE LambdaCase, ViewPatterns, PatternSynonyms #-}
module Text.Parselet where

import Prelude hiding (drop, length, any, repeat)

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

phrase :: Text -> Parser Text
phrase match = Parser $ \s ->
  if match `isPrefixOf` s
  then Just (match, drop (length match) s)
  else Nothing

phrase_ :: Text -> Parser ()
phrase_ t = void (phrase t)

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
  ss@(s :< _) | s == c -> Just ((), ss)
  (_ :< rest) | otherwise -> runParser rest (upto c)
  _ -> Nothing

toNext :: Text -> Parser Text
toNext  s = eatTo (phrase s)

one :: Char -> Parser Char
one c = Parser $ \case
  (s :< rest) | s == c -> Just (s, rest)
  _ -> Nothing

one_ :: Char -> Parser ()
one_ c = void (one c)

any :: [Char] -> Parser Char
any cs = Parser $ \case
  (s :< rest) | s `elem` cs -> Just (s, rest)
  _ -> Nothing

alphanumeric :: Parser Char
alphanumeric = any "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890"

repeat :: Char -> Parser Text
repeat c = fmap pack (repeatUntil (one c))

whitespace :: Parser Text
whitespace = repeat ' '

word :: Parser Text
word = fmap pack (repeatUntil alphanumeric)

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
    go pp s =
      case runParser s pp of
        Just (res, rest) ->
          let (r, rr) = go pp rest
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
    go _ _ = (empty, empty)
