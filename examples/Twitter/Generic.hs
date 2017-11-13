-- Use GHC generics to automatically generate good instances.

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Twitter.Generic
    (
      Metadata(..)
    , Geo(..)
    , Story(..)
    , Result(..)
    ) where

import Prelude ()

import Twitter
import Twitter.Options

import Data.Aeson (ToJSON (..), FromJSON (..), genericToJSON, genericToEncoding, genericParseJSON)

instance ToJSON Metadata
instance FromJSON Metadata

instance ToJSON Geo
instance FromJSON Geo

instance ToJSON Story
instance FromJSON Story

instance ToJSON Result where
    toJSON = genericToJSON twitterOptions
    toEncoding = genericToEncoding twitterOptions
instance FromJSON Result where
    parseJSON = genericParseJSON twitterOptions
