module Main (main) where

import Prelude ()
import Prelude.Compat

import Criterion.Main

-- Encoding is a newtype wrapper around Builder
import qualified Data.Aeson.Encoding as AB (text, string, encodingToLazyByteString)
import qualified Data.Text as T

main :: IO ()
main = do
  let txt = "append (append b (primBounded w1 x1)) (primBounded w2 x2)"
  defaultMain [
    bgroup "string" [
      bench "text" $ nf (AB.encodingToLazyByteString . AB.text) (T.pack txt)
    , bench "string direct" $ nf (AB.encodingToLazyByteString . AB.string) txt
    , bench "string via text" $ nf (AB.encodingToLazyByteString . AB.text . T.pack) txt
    ]
   ]
