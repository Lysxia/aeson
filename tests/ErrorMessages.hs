{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ErrorMessages
  (
    tests
  ) where

import Prelude ()
import Prelude.Compat

import Data.Aeson (FromJSON(..), Value, json)
import Data.Aeson.Types (Parser)
import Data.Aeson.Parser (eitherDecodeWith)
import Data.Aeson.Internal (formatError, iparse)
import Data.Proxy (Proxy(..))
import Data.Semigroup ((<>))
import Instances ()
import Numeric.Natural (Natural)
import Test.Tasty (TestTree)
import Test.Tasty.Golden
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.HashMap.Strict as HM

tests :: [TestTree]
tests =
    [ goldenVsString "simple" "tests/golden/simple.txt" (pure output)
    ]

output :: L.ByteString
output = (L.pack . unlines . concat)
  [ testFor "Int" (Proxy :: Proxy Int)
      [ "\"\""
      , "[]"
      , "{}"
      , "null"
      ]

  , testFor "Integer" (Proxy :: Proxy Integer)
      [ "44.44"
      ]

  , testFor "Natural" (Proxy :: Proxy Natural)
      [ "44.44"
      , "-50"
      ]

  , testFor "String" (Proxy :: Proxy String)
      [ "1"
      , "[]"
      , "{}"
      , "null"
      ]

  , testFor "HashMap" (Proxy :: Proxy (HM.HashMap String Int))
      [ "\"\""
      , "[]"
      ]
  ]

type Output = [String]

outputLine :: String -> Output
outputLine = pure

testWith :: Show a => String -> (Value -> Parser a) -> [L.ByteString] -> Output
testWith name parser ts =
  outputLine name <>
  flip foldMap ts (\s ->
    case eitherDecodeWith json (iparse parser) s of
      Left err -> outputLine $ uncurry formatError err
      Right a -> outputLine $ show a)

testFor :: forall a proxy. (FromJSON a, Show a)
        => String -> proxy a -> [L.ByteString] -> Output
testFor name _ = testWith name (parseJSON :: Value -> Parser a)
