{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ErrorMessages
  (
    tests
  ) where

import Prelude ()
import Prelude.Compat

import Data.Aeson (FromJSON(..), eitherDecode)
import Data.Proxy (Proxy(..))
import Instances ()
import Numeric.Natural (Natural)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase)
import Test.HUnit (Assertion, assertFailure, assertEqual)
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.HashMap.Strict as HM

tests :: [TestTree]
tests =
    [
      testCase "Int" int
    , testCase "Integer" integer
    , testCase "Natural" natural
    , testCase "String" string
    , testCase "HashMap" hashMap
    ]

int :: Assertion
int = do
  let t = test (Proxy :: Proxy Int)
  t "\"\"" $ failed "Int" "String"
  t "[]"   $ failed "Int" "Array"
  t "{}"   $ failed "Int" "Object"
  t "null" $ failed "Int" "Null"

integer :: Assertion
integer = do
  let t = test (Proxy :: Proxy Integer)
  t "44.44" $ failed "Integer" "floating number 44.44"

natural :: Assertion
natural = do
  let t = test (Proxy :: Proxy Natural)
  t "44.44" $ failed "Natural" "floating number 44.44"
  t "-50"   $ failed "Natural" "negative number -50"

string :: Assertion
string = do
  let t = test (Proxy :: Proxy String)
  t "1"    $ expected "String" "Number"
  t "[]"   $ expected "String" "Array"
  t "{}"   $ expected "String" "Object"
  t "null" $ expected "String" "Null"

hashMap :: Assertion
hashMap = do
  let t = test (Proxy :: Proxy (HM.HashMap String Int))
  t "\"\"" $ failed' "HashMap" "Object" "String"
  t "[]"   $ failed' "HashMap" "Object" "Array"

failed :: String -> String -> String
failed ctx enc = "Error in $: parsing " ++ ctx ++ " failed, unexpected " ++ enc

failed' :: String -> String -> String -> String
failed' ctx ex enc =
    "Error in $: parsing " ++ ctx ++
    " failed, expected " ++ ex ++
    ", encountered " ++ enc

expected :: String -> String -> String
expected ex enc = "Error in $: expected " ++ ex ++ ", encountered " ++ enc

test :: forall a proxy . (FromJSON a, Show a) => proxy a -> L.ByteString -> String -> Assertion
test _ v msg = case eitherDecode v of
    Left e -> assertEqual "Invalid error message" msg e
    Right (x :: a) -> assertFailure $ "Expected parsing to fail but it suceeded with: " ++ show x
