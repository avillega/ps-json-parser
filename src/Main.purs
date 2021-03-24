module Main where

import Prelude

import Control.Alt ((<|>))
import Control.Lazy (fix)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple(..))
import Parser (Parser, charP, intP, manyP, sepByP, strLiteralP, tagP, wrappedP, wsP)

data JsonValue
  = JsonNull
  | JsonBool Boolean
  | JsonNumber Int
  | JsonString String
  | JsonArray (Array JsonValue)
  | JsonObject (Array (Tuple String JsonValue))

derive instance genericJsonValue :: Generic JsonValue _

instance showJsonValue :: Show JsonValue where
  show v = genericShow v

jsonNull :: Parser JsonValue
jsonNull = JsonNull <$ tagP "null"

jsonBool :: Parser JsonValue
jsonBool = jsonTrue <|> jsonFalse
  where
  jsonTrue = (JsonBool true) <$ tagP "true"

  jsonFalse = (JsonBool false) <$ tagP "false"

jsonNumber :: Parser JsonValue
jsonNumber = JsonNumber <$> intP

jsonString :: Parser JsonValue
jsonString =
  JsonString
    <$> do
        str <- strLiteralP
        pure str

jsonArray :: Parser JsonValue -> Parser JsonValue
jsonArray p = JsonArray <$> wrappedP open (sepByP p wsAndComma) close
  where
  open = (charP '[') *> (manyP wsP)

  close = (manyP wsP) <* (charP ']')

  wsAndComma = (manyP wsP) *> (charP ',') <* (manyP wsP)

jsonObject :: Parser JsonValue -> Parser JsonValue
jsonObject p = JsonObject <$> wrappedP open (sepByP objPair wsAndComma) close
  where
  open = (charP '{') *> (manyP wsP)

  close = (manyP wsP) <* (charP '}')

  wsAndComma = (manyP wsP) *> (charP ',') <* (manyP wsP)

  objPair = do
    str <- strLiteralP
    _ <- (manyP wsP) *> (charP ':') *> (manyP wsP)
    value <- p
    pure (Tuple str value)

jsonValue :: Parser JsonValue
jsonValue =
  fix
    $ \p ->
        (manyP wsP)
          *> ( jsonNull
                <|> jsonBool
                <|> jsonNumber
                <|> jsonString
                <|> jsonArray p
                <|> jsonObject p
            ) <* (manyP wsP)
