module Lib
    ( run
    ) where

import Text.Parsec
import Text.Parsec.String

data JsValue = JsNumber Int | JsArray [JsValue] | JsTrue | JsFalse | JsNull
    deriving Show

jsValue :: Parser JsValue
jsValue = spaces >> choice [jsNumber, jsArray]

jsNumber :: Parser JsValue
jsNumber = JsNumber . read <$> many1 digit

jsArray :: Parser JsValue
jsArray = JsArray <$> between (char '[') (char ']') (sepBy jsValue (char ','))

jsTrue :: Parser JsValue


run :: String -> String
run = show . parse jsValue ""

