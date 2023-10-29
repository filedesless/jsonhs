import Criterion.Main
import qualified Data.Aeson as A
import Data.Bifunctor
import qualified Data.ByteString.Lazy.Char8 as B
import Lib
import qualified Text.JSON as J

arrays :: String
arrays = replicate 10000 '[' ++ replicate 10000 ']'

longString :: String
longString = "\"" ++ replicate 100000 'a' ++ "\""

main :: IO ()
main =
  defaultMain
    [ bgroup
        "nested arrays"
        [ bench "deserialize" $ nf (bimap (const "") show . deserialize) arrays
        , bench "aeson decode" $ nf (show . aeson_decode . B.pack) arrays
        , bench "json decode" $ nf (show . json_decode) arrays
        , bench "serialize" $ nf (bimap (const "") serialize) (deserialize arrays)
        , bench "aeson encode" $ nf (A.encode <$>) (aeson_decode (B.pack arrays))
        , bench "json encode" $ nf (J.resultToEither . (J.encodeStrict <$>)) (json_decode arrays)
        ]
    , bgroup
        "long strings"
        [ bench "deserialize" $ nf (bimap (const "") show . deserialize) longString
        , bench "aeson decode" $ nf (show . aeson_decode . B.pack) longString
        , bench "json decode" $ nf (show . json_decode) longString
        , bench "serialize" $ nf (bimap (const "") serialize) (deserialize longString)
        , bench "aeson encode" $ nf (A.encode <$>) (aeson_decode (B.pack longString))
        , bench "json encode" $ nf (J.resultToEither . (J.encodeStrict <$>)) (json_decode longString)
        ]
    ]
  where
    aeson_decode = A.decode :: B.ByteString -> Maybe A.Value
    json_decode = J.decodeStrict :: String -> J.Result J.JSValue