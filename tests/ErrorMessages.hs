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

import Encoders
import Types

tests :: [TestTree]
tests =
    [ goldenVsString "simple" "tests/golden/simple.txt" (pure output)
    , goldenVsString "generic" "tests/golden/generic.txt" (pure (outputGeneric G))
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

data Choice = TH | G

outputGeneric :: Choice -> L.ByteString
outputGeneric choice = (L.pack . unlines . concat)
  [ testWith "OneConstructor"
      (select
        thOneConstructorParseJSONDefault
        gOneConstructorParseJSONDefault)
      [ "\"X\""
      , "[0]"
      ]

  , testWith "Nullary"
      (select
        thNullaryParseJSONString
        gNullaryParseJSONString)
      [ "\"X\""
      , "[]"
      ]

  , testWithSomeType "SomeType (tagged)"
      (select
        thSomeTypeParseJSONTaggedObject
        gSomeTypeParseJSONTaggedObject)
      [ "{\"tag\": \"unary\", \"contents\": true}"
      , "{\"tag\": \"unary\"}"
      , "{\"tag\": \"record\"}"
      , "{\"tag\": \"X\"}"
      , "{}"
      , "[]"
      ]

  , testWithSomeType "SomeType (single-field)"
      (select
        thSomeTypeParseJSONObjectWithSingleField
        gSomeTypeParseJSONObjectWithSingleField)
      [ "{\"unary\": {}}"
      , "{\"unary\": []}"
      , "{\"X\": []}"
      , "{\"record\": {}, \"W\":{}}"
      , "{}"
      , "[]"
      ]

  , testWithSomeType "SomeType (two-element array)"
      (select
        thSomeTypeParseJSON2ElemArray
        gSomeTypeParseJSON2ElemArray)
      [ "[\"unary\", true]"
      , "[\"record\", null]"
      , "[\"X\", 0]"
      , "[]"
      , "{}"
      ]

  , testWith "EitherTextInt"
      (select
        thEitherTextIntParseJSONUntaggedValue
        gEitherTextIntParseJSONUntaggedValue)
      [ "\"X\""
      , "[]"
      ]

  , testWith "Product2 Int Bool"
      (select
        thProduct2ParseJSON
        gProduct2ParseJSON)
      [ "[1, null]"
      , "[]"
      , "{}"
      ]
  ]
  where
    select a b = case choice of
      TH -> a
      G -> b

-- Test infrastructure

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

testWithSomeType :: String -> (Value -> Parser (SomeType Int)) -> [L.ByteString] -> Output
testWithSomeType = testWith
