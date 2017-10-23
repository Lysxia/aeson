{-# LANGUAGE BangPatterns, Rank2Types, OverloadedStrings,
    RankNTypes, RecordWildCards, MagicHash, UnboxedTuples #-}

module Data.Aeson.Parser.Attoparsec where

import Control.Monad
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

data S s = Continue {-# UNPACK #-} !Int s | Done {-# UNPACK #-} !Int s

newtype P s a = P {
    runP :: forall r
         .  (Ptr Word8 -> s -> a -> IO r)  -- Read some words
         -> (Ptr Word8 -> s -> IO r)  -- Expected more input
         -> (Ptr Word8 -> s -> IO r)  -- Done
         -> Ptr Word8              -- Start reading from here
         -> s
         -> IO r
  }

instance Functor (P s) where
  fmap = liftM

instance Applicative (P s) where
  pure = return
  (<*>) = ap

instance Monad (P s) where
  return a = P (\k _ _ ptr s -> k ptr s a)

  P run >>= p = P $ \k more done ptr s ->
    run (\ptr' s' a -> runP (p a) k more done ptr' s')
      more
      done
      ptr
      s

scan0_
  :: (s -> [ByteString] -> Parser r)
  -> s
  -- -> (forall r. s -> (Int -> ((S s -> r) -> Word8 -> r) -> ((s -> r) -> r) -> r) -> r)
  -- -> (forall r. s -> (Int -> s -> ((S s -> r) -> Word8 -> r) -> r) -> r)
  -> (forall m. Monad m => s -> (s -> m Word8) -> (Maybe s -> m s) -> m s)
  -> Parser r
scan0_ f s0 p = go [] s0
 where
  go acc s1 = do
    let scanner (B.PS fp off len) =
          withForeignPtr fp $ \ptr0 -> do
            let start = ptr0 `plusPtr` off
                end   = start `plusPtr` len
                done !i !s = return (T i s)
                inner ptr !s =
                  let peek' s' = P $ \k more done ptr' s ->
                        if ptr' < end then do
                          peek ptr' >>= k (ptr' `plusPtr` 1) s'
                        else
                          more ptr' s'
                      done' s_ = P $ \k more done ptr' s ->
                        case s_ of
                          Nothing -> done (ptr' `plusPtr` (-1)) s
                          Just s' -> done  ptr' s'
                  in runP (p s peek' done')
                    (\ptr' _ s' -> inner ptr' s')
                    (\ptr' s' -> done (ptr' `minusPtr` start) s')
                    (\ptr' s' -> done (ptr' `minusPtr` start) s')
                    ptr
                    s
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
  p' s getWord exit = go s
    where
      go s = do
        w <- getWord s
        case p s w of
          Just s' -> go s'
          Nothing -> exit Nothing
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
