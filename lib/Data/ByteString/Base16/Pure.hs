{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Data.ByteString.Base16.Pure
-- Copyright: (c) 2025 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- Pure Haskell base16 encoding and decoding of strict bytestrings.

module Data.ByteString.Base16.Pure (
    encode
  , decode
  ) where

import qualified Data.Bits as B
import Data.Bits ((.&.), (.|.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BI
import Data.Word (Word8, Word16)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
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

-- 256-byte table.  Index by an ASCII byte to obtain its nibble; valid
-- hex chars ('0'..'9', 'a'..'f', 'A'..'F') map to 0x10..0x1f, every
-- other byte maps to 0x20.
--
-- The encoding is chosen so the literal is strictly ASCII and contains
-- no embedded NUL, which is what the bytestring 'IsString' rule needs
-- to rewrite it into 'unsafePackAddress' (cf. 'enc_tab') — the bytes
-- end up in static rodata, with no CAF allocation.
--
-- The 0x20 sentinel is distinguished by bit 5; no value 0x10..0x1f
-- carries that bit, so 'decode' OR-folds every lookup into an
-- accumulator and tests 'acc .&. 0x20 == 0' once at the end.  The
-- output byte is '(n0 `shiftL` 4) .|. (n1 .&. 0x0f)': in 'Word8' the
-- shift naturally drops bit 4, and the mask isolates the low nibble.
dec_tab :: BS.ByteString
dec_tab =
  "\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\
  \\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\
  \\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\
  \\x10\x11\x12\x13\x14\x15\x16\x17\x18\x19\x20\x20\x20\x20\x20\x20\
  \\x20\x1a\x1b\x1c\x1d\x1e\x1f\x20\x20\x20\x20\x20\x20\x20\x20\x20\
  \\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\
  \\x20\x1a\x1b\x1c\x1d\x1e\x1f\x20\x20\x20\x20\x20\x20\x20\x20\x20\
  \\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\
  \\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\
  \\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\
  \\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\
  \\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\
  \\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\
  \\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\
  \\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\
  \\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20"
{-# NOINLINE dec_tab #-}

-- | Encode a base256 'ByteString' as base16.
encode :: BS.ByteString -> BS.ByteString
encode (BI.PS sfp soff l) =
  case enc_tab of
    BI.PS tfp toff _ ->
      BI.unsafeCreate (l `B.shiftL` 1) $ \dst ->
        withForeignPtr sfp $ \sp0 ->
        withForeignPtr tfp $ \tp0 -> do
          -- read 'enc_tab' and write 'dst' as 'Word16' pairs.  The
          -- two-byte block at 'enc_tab[2*b]' and the two-byte block
          -- at 'dst[2*i]' share the same byte layout in memory, so
          -- this is endianness-safe: we never inspect the numerical
          -- value of the 'Word16', we just shuffle 16 bits between
          -- two locations.
          let !sp = sp0 `plusPtr` soff :: Ptr Word8
              !tp = tp0 `plusPtr` toff :: Ptr Word16
              !dp = castPtr dst        :: Ptr Word16
              loop !i
                | i == l    = pure ()
                | otherwise = do
                    b <- peekElemOff sp i
                    w <- peekElemOff tp (fi b)
                    pokeElemOff dp i (w :: Word16)
                    loop (i + 1)
          loop 0

-- | Decode a base16 'ByteString' to base256.  Invalid inputs
--   (including odd-length inputs) will produce 'Nothing'.
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
                          pure $! acc .&. 0x20 == 0
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
