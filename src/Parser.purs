module Parser where

import Prelude

import Control.Alt (class Alt, (<|>))
import Control.Alternative (class Alternative, empty, guard)
import Control.MonadZero (guard)
import Control.Plus (class Plus)
import Data.Array (span, (:))
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (fromCharArray, toCharArray, uncons)
import Data.Traversable (sequenceDefault)
import Data.Tuple (Tuple(..))

newtype Parser a
  = Parser (String -> Maybe (Tuple a String))

instance functorParser :: Functor Parser where
  map func (Parser p) =
    Parser
      $ \input -> do
          (Tuple x rest) <- p input
          Just (Tuple (func x) rest)

instance applyParser :: Apply Parser where
  apply (Parser p1) (Parser p2) =
    Parser
      $ \input -> do
          (Tuple f input') <- p1 input
          (Tuple a input'') <- p2 input'
          Just (Tuple (f a) input'')

instance applicativeParser :: Applicative Parser where
  pure x = Parser $ \input -> Just $ Tuple x input

instance altParser :: Alt Parser where
  alt (Parser p1) (Parser p2) =
    Parser
      $ \input -> (p1 input) <|> (p2 input)

instance plusParser :: Plus Parser where
  empty = Parser $ \_ -> Nothing

instance alternativeParser :: Alternative Parser

instance bindParser :: Bind Parser where
  bind :: forall a b. Parser a -> (a -> Parser b) -> Parser b
  bind (Parser p) f =
    Parser
      $ \input -> do
          (Tuple v input') <- p input
          runParser (f v) input'

instance monadParser :: Monad Parser

runParser :: forall a. Parser a -> String -> Maybe (Tuple a String)
runParser (Parser p) input = do
  result <- p input
  pure result

itemP :: Parser Char
itemP = Parser f
  where
  f input = do
    { head, tail } <- uncons input
    Just (Tuple head tail)

predP :: (Char -> Boolean) -> Parser Char
predP pred = do
  x <- itemP
  guard $ pred x
  pure x

charP :: Char -> Parser Char
charP c = predP (\c' -> c' == c)

digitP :: Parser Char
digitP = predP (\c -> c >= '0' && c <= '9')

upperP :: Parser Char
upperP = predP (\c -> c >= 'A' && c <= 'Z')

lowerP :: Parser Char
lowerP = predP (\c -> c >= 'a' && c <= 'z')

letterP :: Parser Char
letterP = upperP <|> lowerP

alphanumP :: Parser Char
alphanumP = letterP <|> digitP

manyP :: forall a. Parser a -> Parser (Array a)
manyP parser = result <|> pure []
  where
  result = do
    x <- parser
    xs <- (manyP parser)
    pure (x : xs)

many1P :: forall a. Parser a -> Parser (Array a)
many1P parser = do
  x <- parser
  xs <- manyP parser
  pure (x : xs)

-- white space parser
wsP :: Parser  Char
wsP = (charP ' ') <|> (charP '\t')

identifierP :: Parser String
identifierP = fromCharArray <$> many1P alphanumP

sepBy1P :: forall a b. Parser a -> Parser b -> Parser (Array a)
sepBy1P p sep = do
  x <- p
  xs <- manyP (sep *> p)
  pure (x : xs)

sepByP :: forall a b. Parser a -> Parser b -> Parser (Array a)
sepByP p sep = (sepBy1P p sep) <|> pure []

wrappedP :: forall a b c. Parser a -> Parser b -> Parser c -> Parser b
wrappedP open p close = do
  _ <- open
  val <- p
  _ <- close
  pure val

strLiteralP :: Parser String
strLiteralP = do
  x <- wrappedP (charP '"') (manyP (predP (\c -> c /= '"'))) (charP '"')
  pure $ fromCharArray x

natP :: Parser Int
natP = do
  numChars <- many1P digitP
  let numStr = fromCharArray numChars
  case fromString numStr of
    (Just n) -> pure n
    _ -> empty

intP :: Parser Int
intP = do
  f <- neg <|> (pure identity)
  n <- natP
  pure $ f n
  where
    neg = do
      _ <- charP '-'
      pure negate

tagP :: String -> Parser (Array Char)
tagP str = sequenceDefault $ map charP $ toCharArray str
