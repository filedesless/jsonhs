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
        JsonObject <$> scale (`div` 2) arbitrary,
        elements [JsonTrue, JsonFalse, JsonNull]
      ]
  shrink (JsonArray l) = JsonNull : l
  shrink (JsonObject l) = JsonNull : map snd l ++ map (JsonString . fst) l
  shrink _ = []

main :: IO ()
main = hspec $ do
  describe "Lib.jsonNumber" $ do
    it "parses test numbers correctly" $ do
      parse (jsonNumber <* eof) "" "0" `shouldBe` Right 0
      parse (jsonNumber <* eof) "" "-0" `shouldBe` Right 0
      parse (jsonNumber <* eof) "" "-1" `shouldBe` Right (-1)
      parse (jsonNumber <* eof) "" "1.0" `shouldBe` Right 1
  describe "Lib.jsonString" $ do
    it "parses test strings correctly" $ do
      parse (jsonString <* eof) "" (show "") `shouldBe` Right ""
      parse (jsonString <* eof) "" (show "abc123") `shouldBe` Right "abc123"
      parse (jsonString <* eof) "" (show "\\abc") `shouldBe` Right "\\abc"
      parse (jsonString <* eof) "" (show "abc\"") `shouldBe` Right "abc\""
      parse (jsonString <* eof) "" (show "\b") `shouldBe` Right "\b"
    it "fails on incorrect strings" $ do
      parse (jsonString <* eof) "" "" `shouldSatisfy` isLeft
      parse (jsonString <* eof) "" "\"\\a\"" `shouldSatisfy` isLeft
    it "parses aeson encoded strings correctly" $
      property $
        \(ASCIIString s) -> parse (jsonString <* eof) "" (B.unpack (encode s)) `shouldBe` Right s

  describe "Lib.jsonObject" $ do
    it "parses test objects correctly" $ do
      parse (jsonObject <* eof) "" "{}" `shouldBe` Right []
      parse (jsonObject <* eof) "" "{ }" `shouldBe` Right []
      parse (jsonObject <* eof) "" "{ \"\" : null }" `shouldBe` Right [("", JsonNull)]

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