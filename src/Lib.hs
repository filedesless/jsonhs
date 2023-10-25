{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Lib where

import Data.Char
import Data.List
import Text.Parsec
import Text.Parsec.String
import Text.Printf

data JsonValue
  = JsonString String
  | JsonNumber Double
  | JsonObject [(String, JsonValue)]
  | JsonArray [JsonValue]
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
jsonValue = do
  spaces
  val <-
    choice
      [ JsonString <$> jsonString
      , JsonNumber <$> jsonNumber
      , JsonObject <$> jsonObject
      , JsonArray <$> jsonArray
      , JsonTrue <$ string "true"
      , JsonFalse <$ string "false"
      , JsonNull <$ string "null"
      ]
  spaces
  return val

jsonString :: Parser String
jsonString =
  between (char '"') (char '"')
    $ many
    $ noneOf ['"', '\\']
    <|> ( char '\\'
            >> ( (jsonEscape <$> satisfy (`elem` jsonEscapeList))
                  <|> parseHex
                  <$> (char 'u' >> count 4 hexDigit)
               )
        )
 where
  parseHex = chr . foldl (\num d -> num * 16 + digitToInt d) 0

jsonNumber :: Parser Double
jsonNumber =
  read
    <$> mconcat
      [ option "" $ string "-"
      , string "0" <|> (:) <$> oneOf ['1' .. '9'] <*> many digit
      , option "" $ string "." <> many1 digit
      , option "" $ mconcat [singleton <$> oneOf "eE", choice (string <$> ["+", "-", ""]), many1 digit]
      ]

jsonObject :: Parser [(String, JsonValue)]
jsonObject = between (char '{') (char '}') (spaces >> sepBy jsonKeyValue (char ','))

jsonKeyValue :: Parser (String, JsonValue)
jsonKeyValue = do
  spaces
  key <- jsonString
  spaces
  _ <- char ':'
  value <- jsonValue
  return (key, value)

jsonArray :: Parser [JsonValue]
jsonArray = between (char '[') (char ']') (sepBy jsonValue (char ','))

deserialize :: String -> Either ParseError JsonValue
deserialize = parse ((JsonString "" <$ eof) <|> jsonValue <* eof) ""

serialize :: JsonValue -> String
serialize (JsonString s) = concat ["\"", s >>= jsonUnEscape, "\""]
serialize (JsonNumber n) = show n
serialize (JsonArray l) = concat ["[", intercalate ", " (map serialize l), "]"]
serialize JsonTrue = "true"
serialize JsonFalse = "false"
serialize JsonNull = "null"
serialize (JsonObject l) = concat ["{", intercalate ", " objects, "}"]
 where
  objects = [serialize (JsonString s) ++ ": " ++ serialize v | (s, v) <- l]