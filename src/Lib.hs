{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Lib where

import Data.Char
import Data.List (intercalate)
import Text.Parsec
import Text.Parsec.String
import Text.Printf

data JsonValue
  = JsonString String
  | JsonNumber Int
  | -- | JsonObject [(String, JsonValue)]
    JsonArray [JsonValue]
  | JsonTrue
  | JsonFalse
  | JsonNull
  deriving (Show, Eq)

jsonEscapeList :: [Char]
jsonEscapeList = ['\"', '\\', '/', 'b', 'f', 'n', 'r', 't']

jsonEscape :: Char -> Char
jsonEscape '\"' = '\"'
jsonEscape '\\' = '\\'
jsonEscape '/' = '/'
jsonEscape 'b' = '\b'
jsonEscape 'f' = '\f'
jsonEscape 'n' = '\n'
jsonEscape 'r' = '\r'
jsonEscape 't' = '\t'
jsonEscape _ = undefined

-- FIXME: weird for aeson compat
jsonUnEscape :: Char -> String
jsonUnEscape '\"' = "\\\""
jsonUnEscape '\\' = "\\\\"
-- jsonUnEscape '\b' = "\\b"
-- jsonUnEscape '\f' = "\\f"
jsonUnEscape '\n' = "\\n"
jsonUnEscape '\r' = "\\r"
jsonUnEscape '\t' = "\\t"
jsonUnEscape '\DEL' = "\DEL"
jsonUnEscape c
  | isControl c = printf "\\u%04x" c
  | otherwise = [c]

jsonValue :: Parser JsonValue
jsonValue = spaces >> choice [jsonNumber, jsonArray, jsonTrue, jsonFalse, jsonNull, jsonString] <* spaces

jsonString :: Parser JsonValue
jsonString =
  fmap JsonString $
    between (char '"') (char '"') $
      many $
        noneOf ['"', '\\']
          <|> ( char '\\'
                  >> ( (jsonEscape <$> satisfy (`elem` jsonEscapeList))
                         <|> parseHex <$> (char 'u' >> count 4 hexDigit)
                     )
              )
  where
    parseHex = chr . foldl (\num d -> num * 16 + digitToInt d) 0

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
deserialize = parse ((JsonString "" <$ eof) <|> jsonValue <* eof) ""

serialize :: JsonValue -> String
serialize (JsonString s) = concat ["\"", s >>= jsonUnEscape, "\""]
serialize (JsonNumber n) = show n
-- serialize (JsonObject l) = undefined
serialize (JsonArray l) = concat ["[", intercalate ", " (map serialize l), "]"]
serialize JsonTrue = "true"
serialize JsonFalse = "false"
serialize JsonNull = "null"