module Main where

import Prelude
import Control.Alt ((<|>))
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple)
import Parser (Parser, intP, strLiteralP, tagP)

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

jsonValue :: Parser JsonValue
jsonValue = jsonNull <|> jsonBool <|> jsonNumber <|> jsonString
