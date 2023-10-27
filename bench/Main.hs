import Criterion.Main
import Data.Aeson
import Data.Bifunctor
import qualified Data.ByteString.Lazy.Char8 as B
import Lib

objects :: String
objects = replicate 1000 '{' ++ replicate 1000 '}'

arrays :: String
arrays = replicate 1000 '[' ++ replicate 1000 ']'

longString :: String
longString = "\"" ++ replicate 100000 'a' ++ "\""

main :: IO ()
main =
  defaultMain
    [ bgroup
        "deserialize"
        [ bench "1000 arrays" $ nf (bimap (const "") serialize . deserialize) arrays
        , bench "1000 objects" $ nf (bimap (const "") serialize . deserialize) objects
        , bench "long string" $ nf (bimap (const "") serialize . deserialize) longString
        ]
    , bgroup
        "aeson decode"
        [ bench "100 arrays" $ nf (encode . (decode :: B.ByteString -> Maybe Value) . B.pack) arrays
        , bench "100 objects" $ nf (encode . (decode :: B.ByteString -> Maybe Value) . B.pack) objects
        , bench "long string" $ nf (encode . (decode :: B.ByteString -> Maybe Value) . B.pack) longString
        ]
    ]
