{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Char
import Data.Either
import Lib
import Test.Hspec
import Test.QuickCheck
import Text.Parsec

instance Arbitrary JsonValue where
  arbitrary =
    oneof
      [ JsonString <$> listOf (arbitraryASCIIChar `suchThat` (not . isControl)),
        JsonNumber <$> arbitrary,
        JsonArray <$> scale (`div` 2) arbitrary,
        elements [JsonTrue, JsonFalse, JsonNull]
      ]

main :: IO ()
main = hspec $ do
  describe "Lib.jsonString" $ do
    it "parses test strings correctly" $ do
      parse (jsonString <* eof) "" (show "") `shouldBe` Right (JsonString "")
      parse (jsonString <* eof) "" (show "abc123") `shouldBe` Right (JsonString "abc123")
      parse (jsonString <* eof) "" (show "\\abc") `shouldBe` Right (JsonString "\\abc")
      parse (jsonString <* eof) "" (show "abc\"") `shouldBe` Right (JsonString "abc\"")
      parse (jsonString <* eof) "" (show "\b") `shouldBe` Right (JsonString "\b")
    it "fails on incorrect strings" $ do
      parse (jsonString <* eof) "" "" `shouldSatisfy` isLeft
      parse (jsonString <* eof) "" "\"\\a\"" `shouldSatisfy` isLeft
    it "parses aeson encoded strings correctly" $
      property $
        \(ASCIIString s) -> parse (jsonString <* eof) "" (B.unpack (encode s)) `shouldBe` Right (JsonString s)

  describe "Lib.serialize" $ do
    it "works on test data" $ do
      serialize (JsonArray []) `shouldBe` "[]"
      serialize (JsonArray [JsonTrue, JsonFalse]) `shouldBe` "[true, false]"
      serialize (JsonString "") `shouldBe` "\"\""
      serialize (JsonString "abc\"def\\") `shouldBe` "\"abc\\\"def\\\\\""
    it "serializes a deserialized aeson encoded string" $
      property $
        \(ASCIIString s) ->
          let expected = B.unpack (encode s) in
            serialize <$> deserialize expected `shouldBe` Right expected

  describe "Lib.deserialize" $ do
    it "deserializes a serialized value" $
      property $
        \val -> deserialize (serialize val) `shouldBe` Right val