{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:      Data.Aeson.Internal.Time
-- Copyright:   (c) 2015-2016 Bryan O'Sullivan
-- License:     BSD3
-- Maintainer:  Bryan O'Sullivan <bos@serpentine.com>
-- Stability:   experimental
-- Portability: portable

module Data.Attoparsec.Time.Internal
    (
      TimeOfDay64(..)
    , fromPico
    , toPico
    , diffTimeOfDay64
    , toTimeOfDay64
    ) where

import Prelude.Compat

import Data.Int (Int64)
import Data.Time

#if !MIN_VERSION_time(1,6,0) || !MIN_VERSION_base(4,7,0)

import Unsafe.Coerce (unsafeCoerce)

#endif

#if MIN_VERSION_time(1,6,0)

import Data.Time.Clock (diffTimeToPicoseconds)

#endif

#if MIN_VERSION_base(4,7,0)

import Data.Fixed (Pico, Fixed(MkFixed))

#else

import Data.Fixed (Pico)

#endif

#if !MIN_VERSION_time(1,6,0)

diffTimeToPicoseconds :: DiffTime -> Integer
diffTimeToPicoseconds = unsafeCoerce

#endif

#if MIN_VERSION_base(4,7,0)

toPico :: Integer -> Pico
toPico = MkFixed

fromPico :: Pico -> Integer
fromPico (MkFixed i) = i

#else

toPico :: Integer -> Pico
toPico = unsafeCoerce

fromPico :: Pico -> Integer
fromPico = unsafeCoerce

#endif

-- | Like TimeOfDay, but using a fixed-width integer for seconds.
data TimeOfDay64 = TOD {-# UNPACK #-} !Int
                       {-# UNPACK #-} !Int
                       {-# UNPACK #-} !Int64

posixDayLength :: DiffTime
posixDayLength = 86400

diffTimeOfDay64 :: DiffTime -> TimeOfDay64
diffTimeOfDay64 t
  | t >= posixDayLength = TOD 23 59 (60000000000000 + pico (t - posixDayLength))
  | otherwise = TOD (fromIntegral h) (fromIntegral m) s
    where (h,mp) = pico t `quotRem` 3600000000000000
          (m,s)  = mp `quotRem` 60000000000000
          pico   = fromIntegral . diffTimeToPicoseconds

toTimeOfDay64 :: TimeOfDay -> TimeOfDay64
toTimeOfDay64 (TimeOfDay h m s) = TOD h m (fromIntegral (fromPico s))
