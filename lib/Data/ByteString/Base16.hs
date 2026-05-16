{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Data.ByteString.Base16
-- Copyright: (c) 2025 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- Pure base16 encoding and decoding of strict bytestrings.

module Data.ByteString.Base16 (
    encode
  , decode
  ) where

import qualified Data.Bits as B
import Data.Bits ((.&.), (.|.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BI
import Data.Word (Word8)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (peekElemOff, pokeElemOff)
import System.IO.Unsafe (unsafeDupablePerformIO)

fi :: (Num a, Integral b) => b -> a
fi = fromIntegral
{-# INLINE fi #-}

-- 512-byte table.  Bytes [2k] and [2k+1] are the two lowercase ASCII
-- hex characters representing the value k.  All-ASCII content means
-- the bytestring 'IsString' rule rewrites this to 'unsafePackAddress'
-- and the bytes live in static rodata.
enc_tab :: BS.ByteString
enc_tab =
  "000102030405060708090a0b0c0d0e0f\
  \101112131415161718191a1b1c1d1e1f\
  \202122232425262728292a2b2c2d2e2f\
  \303132333435363738393a3b3c3d3e3f\
  \404142434445464748494a4b4c4d4e4f\
  \505152535455565758595a5b5c5d5e5f\
  \606162636465666768696a6b6c6d6e6f\
  \707172737475767778797a7b7c7d7e7f\
  \808182838485868788898a8b8c8d8e8f\
  \909192939495969798999a9b9c9d9e9f\
  \a0a1a2a3a4a5a6a7a8a9aaabacadaeaf\
  \b0b1b2b3b4b5b6b7b8b9babbbcbdbebf\
  \c0c1c2c3c4c5c6c7c8c9cacbcccdcecf\
  \d0d1d2d3d4d5d6d7d8d9dadbdcdddedf\
  \e0e1e2e3e4e5e6e7e8e9eaebecedeeef\
  \f0f1f2f3f4f5f6f7f8f9fafbfcfdfeff"
{-# NOINLINE enc_tab #-}

-- 256-byte table.  Index by an ASCII byte to obtain its nibble value
-- (0..15); invalid bytes map to 0x80.  Built once at first use, with
-- no intermediate list (which would balloon CAF allocation).
dec_tab :: BS.ByteString
dec_tab = BI.unsafeCreate 256 $ \p ->
  let go !i
        | i == 256  = pure ()
        | otherwise = pokeElemOff p i (nib i) >> go (i + 1)
  in  go 0
  where
    nib :: Int -> Word8
    nib c
      | c >= 0x30 && c <= 0x39 = fi (c - 0x30) -- '0'..'9'
      | c >= 0x41 && c <= 0x46 = fi (c - 0x37) -- 'A'..'F'
      | c >= 0x61 && c <= 0x66 = fi (c - 0x57) -- 'a'..'f'
      | otherwise              = 0x80
{-# NOINLINE dec_tab #-}

-- | Encode a base256 'ByteString' as base16.
--
--   >>> encode "hello world"
--   "68656c6c6f20776f726c64"
encode :: BS.ByteString -> BS.ByteString
encode (BI.PS sfp soff l) =
  case enc_tab of
    BI.PS tfp toff _ ->
      BI.unsafeCreate (l `B.shiftL` 1) $ \dst ->
        withForeignPtr sfp $ \sp0 ->
        withForeignPtr tfp $ \tp0 -> do
          let !sp = sp0 `plusPtr` soff :: Ptr Word8
              !tp = tp0 `plusPtr` toff :: Ptr Word8
              loop !i
                | i == l    = pure ()
                | otherwise = do
                    b <- peekElemOff sp i
                    let !j = fi b `B.shiftL` 1
                        !o = i `B.shiftL` 1
                    hi <- peekElemOff tp j
                    lo <- peekElemOff tp (j + 1)
                    pokeElemOff dst  o      hi
                    pokeElemOff dst (o + 1) lo
                    loop (i + 1)
          loop 0

-- | Decode a base16 'ByteString' to base256.
--
--   Invalid inputs (including odd-length inputs) will produce
--   'Nothing'.
--
--   >>> decode "68656c6c6f20776f726c64"
--   Just "hello world"
--   >>> decode "068656c6c6f20776f726c64" -- odd-length
--   Nothing
decode :: BS.ByteString -> Maybe BS.ByteString
decode (BI.PS sfp soff l)
  | B.testBit l 0 = Nothing
  | otherwise = case dec_tab of
      BI.PS tfp toff _ -> unsafeDupablePerformIO $ do
        let !n = l `B.shiftR` 1
        fp <- BI.mallocByteString n
        ok <- withForeignPtr fp  $ \dst ->
              withForeignPtr sfp $ \sp0 ->
              withForeignPtr tfp $ \tp0 -> do
                let !sp = sp0 `plusPtr` soff :: Ptr Word8
                    !tp = tp0 `plusPtr` toff :: Ptr Word8
                    loop !i !acc
                      | i == n    =
                          pure $! acc .&. 0x80 == 0
                      | otherwise = do
                          let !o = i `B.shiftL` 1
                          c0 <- peekElemOff sp  o
                          c1 <- peekElemOff sp (o + 1)
                          n0 <- peekElemOff tp (fi c0)
                          n1 <- peekElemOff tp (fi c1)
                          let !b = (n0 `B.shiftL` 4)
                               .|. (n1 .&. 0x0f)
                          pokeElemOff dst i b
                          loop (i + 1) (acc .|. n0 .|. n1)
                loop 0 0
        pure $! if ok then Just (BI.PS fp 0 n) else Nothing
