{-# LANGUAGE BangPatterns, Rank2Types, OverloadedStrings,
    RankNTypes, RecordWildCards, MagicHash, UnboxedTuples #-}

module Data.Aeson.Parser.Attoparsec where

import Data.Attoparsec.Internal (concatReverse)
import Data.Attoparsec.ByteString.Internal
import Data.ByteString (ByteString)
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Unsafe as B
import Data.Word (Word8)

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable

import GHC.Base (realWorld#)
import GHC.IO (IO(IO))


data T s = T {-# UNPACK #-} !Int s

data S s = More s | Continue {-# UNPACK #-} !Int s | Done {-# UNPACK #-} !Int s

scan0_
  :: (s -> [ByteString] -> Parser r)
  -> s
  -> (forall r. s -> (Int -> ((S s -> r) -> Word8 -> r) -> ((S s -> r) -> r) -> r) -> r)
  -> Parser r
scan0_ f s0 p = go [] s0
 where
  go acc s1 = do
    let scanner (B.PS fp off len) =
          withForeignPtr fp $ \ptr0 -> do
            let start = ptr0 `plusPtr` off
                end   = start `plusPtr` len
                inner ptr !s = do
                  let peek' i ok no =  do
                        let ptr' = ptr `plusPtr` i
                        if ptr' < end then
                          peek (ptr' `plusPtr` i) >>= ok ret
                        else
                          no ret
                      ret s_ =
                        case s_ of
                          More s' -> done (end `minusPtr` start) s'
                          Continue i' s' -> inner (ptr `plusPtr` i') s'
                          Done i' s' -> done (ptr `plusPtr` i' `minusPtr` start) s'
                  p s peek'
                done !i !s = return (T i s)
            inner start s1
    bs <- get
    let T i s' = inlinePerformIO $ scanner bs
        !h = B.unsafeTake i bs
    continue <- inputSpansChunks i
    if continue
      then go (h:acc) s'
      else f s' (h:acc)
{-# INLINE scan0_ #-}

scan_ :: (s -> [ByteString] -> Parser r) -> s -> (s -> Word8 -> Maybe s)
         -> Parser r
scan_ f s0 p = scan0_ f s0 p'
 where
  p' s peek' = do
    peek' 0
      (\ret w -> ret $ case p s w of
        Just s' -> Continue 1 s'
        Nothing -> Done 0 s)
      (\ret -> ret (More s))
{-# INLINE scan_ #-}

runScanner :: s -> (s -> Word8 -> Maybe s) -> Parser (ByteString, s)
runScanner = scan_ $ \s xs -> let !sx = concatReverse xs in return (sx, s)
{-# INLINE runScanner #-}

-- | Just like unsafePerformIO, but we inline it. Big performance gains as
-- it exposes lots of things to further inlining. /Very unsafe/. In
-- particular, you should do no memory allocation inside an
-- 'inlinePerformIO' block. On Hugs this is just @unsafePerformIO@.
inlinePerformIO :: IO a -> a
inlinePerformIO (IO m) = case m realWorld# of (# _, r #) -> r
{-# INLINE inlinePerformIO #-}
