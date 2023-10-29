{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Lib where

import Data.Char
import Data.List (singleton, intersperse)
import Text.Parsec
import Text.Parsec.String
import Text.Printf

data JsonValue
  = JsonString String
  | JsonNumber Double
  | JsonObject [(String, JsonValue)] -- TODO: Map
  | JsonArray [JsonValue] -- TODO: Vector
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
      [ JsonArray <$> jsonArray
      , JsonObject <$> jsonObject
      , JsonString <$> jsonString
      , JsonNumber <$> jsonNumber
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

showJsonValue :: JsonValue -> ShowS
showJsonValue (JsonArray l) = showChar '[' . foldr (.) id (intersperse (showString ", ") (showJsonValue <$> l)) . showChar ']'
showJsonValue (JsonObject l) = showChar '{' . foldr (.) id (intersperse (showString ", ") (showJsonObject <$> l)) . showChar '}'
  where showJsonObject (k, v) = showJsonValue (JsonString k) . showString ": " . showJsonValue v
showJsonValue (JsonNumber n) = showString (show n)
showJsonValue (JsonString s) = showChar '\"' . showString (s >>= jsonUnEscape) . showChar '\"'
showJsonValue JsonTrue = showString "true"
showJsonValue JsonFalse = showString "false"
showJsonValue JsonNull = showString "null"

serialize :: JsonValue -> String
serialize v = showJsonValue v []