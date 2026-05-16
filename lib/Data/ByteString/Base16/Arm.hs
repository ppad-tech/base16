{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE BangPatterns #-}

-- |
-- Module: Data.ByteString.Base16.Arm
-- Copyright: (c) 2025 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- ARM NEON support for base16 encoding and decoding.

module Data.ByteString.Base16.Arm (
    base16_arm_available
  , encode
  , decode
  ) where

import qualified Data.Bits as B
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BI
import Data.Word (Word8)
import Foreign.C.Types (CInt(..), CSize(..))
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (Ptr, plusPtr)
import System.IO.Unsafe (unsafeDupablePerformIO)

-- ffi ------------------------------------------------------------------------

foreign import ccall unsafe "base16_encode_arm"
  c_base16_encode :: Ptr Word8 -> Ptr Word8 -> CSize -> IO ()

foreign import ccall unsafe "base16_decode_arm"
  c_base16_decode :: Ptr Word8 -> Ptr Word8 -> CSize -> IO CInt

foreign import ccall unsafe "base16_arm_available"
  c_base16_arm_available :: IO CInt

-- utilities ------------------------------------------------------------------

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral
{-# INLINE fi #-}

-- api ------------------------------------------------------------------------

-- | Are ARM NEON extensions available?
base16_arm_available :: Bool
base16_arm_available =
  unsafeDupablePerformIO c_base16_arm_available /= 0
{-# NOINLINE base16_arm_available #-}

-- | Encode a base256 'ByteString' as base16 using NEON.
encode :: BS.ByteString -> BS.ByteString
encode (BI.PS sfp soff l) =
  BI.unsafeCreate (l `B.shiftL` 1) $ \dst ->
    withForeignPtr sfp $ \sp0 ->
      c_base16_encode (sp0 `plusPtr` soff) dst (fi l)

-- | Decode a base16 'ByteString' to base256 using NEON.  Returns
--   'Nothing' on odd-length or otherwise invalid input.
decode :: BS.ByteString -> Maybe BS.ByteString
decode (BI.PS sfp soff l)
  | B.testBit l 0 = Nothing
  | otherwise = unsafeDupablePerformIO $ do
      let !n = l `B.shiftR` 1
      fp <- BI.mallocByteString n
      ok <- withForeignPtr fp  $ \dst ->
            withForeignPtr sfp $ \sp0 ->
              c_base16_decode (sp0 `plusPtr` soff) dst (fi n)
      pure $! if ok /= 0 then Just (BI.PS fp 0 n) else Nothing
