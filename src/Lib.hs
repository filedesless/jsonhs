module Lib (deserialize, JsonValue (..), serialize) where

import Text.Parsec
import Text.Parsec.String
import Data.List (intercalate)

data JsonValue
  = JsonString String
  | JsonNumber Int
  -- | JsonObject [(String, JsonValue)]
  | JsonArray [JsonValue]
  | JsonTrue
  | JsonFalse
  | JsonNull
  deriving (Show, Eq)

jsonValue :: Parser JsonValue
jsonValue = spaces >> choice [jsonNumber, jsonArray, jsonTrue, jsonFalse, jsonNull, jsonString] <* spaces

-- TODO: \ escaping
jsonString :: Parser JsonValue
jsonString = JsonString <$> between (char '"') (char '"') (many (noneOf "\""))

-- TODO: read floating point numbers
jsonNumber :: Parser JsonValue
jsonNumber = JsonNumber . read <$> option "" (return <$> char '-') <> many1 digit

-- jsonObject :: Parser JsonValue
-- jsonObject = fail "not impemented"

jsonArray :: Parser JsonValue
jsonArray = JsonArray <$> between (char '[') (char ']') (sepBy jsonValue (char ','))

jsonTrue :: Parser JsonValue
jsonTrue = JsonTrue <$ string "true"

jsonFalse :: Parser JsonValue
jsonFalse = JsonFalse <$ string "false"

jsonNull :: Parser JsonValue
jsonNull = JsonNull <$ string "null"

deserialize :: String -> Either ParseError JsonValue
deserialize = parse (jsonValue <* eof) ""

serialize :: JsonValue -> String
serialize (JsonString s) = '"' : s ++ "\""
serialize (JsonNumber n) = show n
-- serialize (JsonObject l) = undefined
serialize (JsonArray l) = "[" ++ intercalate ", " (map serialize l) ++ "]"
serialize JsonTrue = "true"
serialize JsonFalse = "false"
serialize JsonNull = "null"