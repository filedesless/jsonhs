{-# OPTIONS_GHC -Wno-orphans #-}

import Lib
import Test.Hspec
import Test.QuickCheck

instance Arbitrary JsonValue where
  arbitrary = oneof [
    JsonString <$> arbitrary,
    JsonNumber <$> arbitrary,
    JsonArray <$> arbitrary,
    elements [JsonTrue, JsonFalse, JsonNull]]

main :: IO ()
main = hspec $ do
  describe "Lib.deserialize" $ do
    it "deserializes what was serialized" $
      property $ \val -> deserialize (serialize val) `shouldBe` Right val