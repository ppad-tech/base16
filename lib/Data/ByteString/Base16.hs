{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BinaryLiterals #-}
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
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Builder.Extra as BE
import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString.Unsafe as BU
import Data.Word (Word8, Word16)

to_strict :: BSB.Builder -> BS.ByteString
to_strict = BS.toStrict . BSB.toLazyByteString
{-# INLINE to_strict #-}

to_strict_small :: BSB.Builder -> BS.ByteString
to_strict_small = BS.toStrict
  . BE.toLazyByteStringWith (BE.safeStrategy 128 BE.smallChunkSize) mempty
{-# INLINE to_strict_small #-}

fi :: (Num a, Integral b) => b -> a
fi = fromIntegral
{-# INLINE fi #-}

hex_charset :: BS.ByteString
hex_charset = "0123456789abcdef"

expand_w8 :: Word8 -> Word16
expand_w8 b =
  let !hi = BU.unsafeIndex hex_charset (fi b `B.shiftR` 4)
      !lo = BU.unsafeIndex hex_charset (fi b .&. 0b00001111)
  in      fi hi `B.shiftL` 8
      .|. fi lo
{-# INLINE expand_w8 #-}

-- | Encode a base256 'ByteString' as base16.
--
--   >>> encode "hello world"
--   "68656c6c6f20776f726c64"
encode :: BS.ByteString -> BS.ByteString
encode bs@(BI.PS _ _ l)
    | l < 64    = to_strict_small loop
    | otherwise = to_strict loop
  where
    -- writing as few words as possible requires performing some length
    -- checks up front
    loop
      | l `rem` 4 == 0 =
               go64 bs
      | (l - 3) `rem` 4 == 0 = case BS.splitAt (l - 3) bs of
          (chunk, etc) ->
               go64 chunk
            <> go32 (BU.unsafeTake 2 etc)
            <> go16 (BU.unsafeDrop 2 etc)
      | (l - 2) `rem` 4 == 0 = case BS.splitAt (l - 2) bs of
          (chunk, etc) ->
               go64 chunk
            <> go32 etc
      | (l - 1) `rem` 4 == 0 = case BS.splitAt (l - 1) bs of
          (chunk, etc) ->
               go64 chunk
            <> go16 etc

      | l `rem` 2 == 0 =
               go32 bs
      | (l - 1) `rem` 2 == 0 = case BS.splitAt (l - 1) bs of
          (chunk, etc) ->
               go32 chunk
            <> go16 etc

      | otherwise =
               go16 bs

    go64 b = case BS.splitAt 4 b of
      (chunk, etc)
        | BS.null chunk -> mempty
        | otherwise ->
            let !w16_0 = expand_w8 (BU.unsafeIndex chunk 0)
                !w16_1 = expand_w8 (BU.unsafeIndex chunk 1)
                !w16_2 = expand_w8 (BU.unsafeIndex chunk 2)
                !w16_3 = expand_w8 (BU.unsafeIndex chunk 3)

                !w64 = fi w16_0 `B.shiftL` 48
                   .|. fi w16_1 `B.shiftL` 32
                   .|. fi w16_2 `B.shiftL` 16
                   .|. fi w16_3

            in  BSB.word64BE w64 <> go64 etc

    go32 b = case BS.splitAt 2 b of
      (chunk, etc)
        | BS.null chunk -> mempty
        | otherwise ->
            let !w16_0 = expand_w8 (BU.unsafeIndex chunk 0)
                !w16_1 = expand_w8 (BU.unsafeIndex chunk 1)

                !w32 = fi w16_0 `B.shiftL` 16
                   .|. fi w16_1

            in  BSB.word32BE w32 <> go32 etc

    go16 b = case BS.uncons b of
      Nothing -> mempty
      Just (h, t) ->
        let !w16 = expand_w8 h
        in  BSB.word16BE w16 <> go16 t

word4 :: Word8 -> Maybe Word8
word4 w8 = fmap fi (BS.elemIndex w8 hex_charset)
{-# INLINE word4 #-}

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
decode bs@(BI.PS _ _ l)
    | B.testBit l 0    = Nothing
    | l `quot` 2 < 128 = fmap to_strict_small loop
    | otherwise        = fmap to_strict loop
  where
    -- same story, but we need more checks
    loop
      | l `rem` 16 == 0 =
            go64 mempty bs
      | (l - 2) `rem` 16 == 0 = case BS.splitAt (l - 2) bs of
          (chunk, etc) -> do
            b0 <- go64 mempty chunk
            go8 b0 etc
      | (l - 4) `rem` 16 == 0 = case BS.splitAt (l - 4) bs of
          (chunk, etc) -> do
            b0 <- go64 mempty chunk
            go16 b0 etc
      | (l - 6) `rem` 16 == 0 = case BS.splitAt (l - 6) bs of
          (chunk, etc) -> do
            b0 <- go64 mempty chunk
            b1 <- go16 b0 (BU.unsafeTake 4 etc)
            go8 b1 (BU.unsafeDrop 4 etc)
      | (l - 8) `rem` 16 == 0 = case BS.splitAt (l - 8) bs of
          (chunk, etc) -> do
            b0 <- go64 mempty chunk
            go32 b0 etc
      | (l - 10) `rem` 16 == 0 = case BS.splitAt (l - 10) bs of
          (chunk, etc) -> do
            b0 <- go64 mempty chunk
            b1 <- go32 b0 (BU.unsafeTake 8 etc)
            go8 b1 (BU.unsafeDrop 8 etc)
      | (l - 12) `rem` 16 == 0 = case BS.splitAt (l - 12) bs of
          (chunk, etc) -> do
            b0 <- go64 mempty chunk
            b1 <- go32 b0 (BU.unsafeTake 8 etc)
            go16 b1 (BU.unsafeDrop 8 etc)
      | (l - 14) `rem` 16 == 0 = case BS.splitAt (l - 14) bs of
          (chunk, etc) -> do
            b0 <- go64 mempty chunk
            b1 <- go32 b0 (BU.unsafeTake 8 etc)
            b2 <- go16 b1 (BU.unsafeTake 4 (BU.unsafeDrop 8 etc))
            go8 b2 (BU.unsafeDrop 12 etc)

      | l `rem` 8 == 0 =
            go32 mempty bs
      | (l - 2) `rem` 8 == 0 = case BS.splitAt (l - 2) bs of
          (chunk, etc) -> do
            b0 <- go32 mempty chunk
            go8 b0 etc
      | (l - 4) `rem` 8 == 0 = case BS.splitAt (l - 4) bs of
          (chunk, etc) -> do
            b0 <- go32 mempty chunk
            go16 b0 etc
      | (l - 6) `rem` 8 == 0 = case BS.splitAt (l - 6) bs of
          (chunk, etc) -> do
            b0 <- go32 mempty chunk
            b1 <- go16 b0 (BU.unsafeTake 4 etc)
            go8 b1  (BU.unsafeDrop 4 etc)

      | l `rem` 4 == 0 =
            go16 mempty bs
      | (l - 2) `rem` 4 == 0 = case BS.splitAt (l - 2) bs of
          (chunk, etc) -> do
            b0 <- go16 mempty chunk
            go8 b0 etc

      | otherwise =
            go8 mempty bs

    go64 acc b = case BS.splitAt 16 b of
      (chunk, etc)
        | BS.null chunk -> pure acc
        | otherwise -> do
            !w4_00 <- word4 (BU.unsafeIndex chunk 00)
            !w4_01 <- word4 (BU.unsafeIndex chunk 01)
            !w4_02 <- word4 (BU.unsafeIndex chunk 02)
            !w4_03 <- word4 (BU.unsafeIndex chunk 03)
            !w4_04 <- word4 (BU.unsafeIndex chunk 04)
            !w4_05 <- word4 (BU.unsafeIndex chunk 05)
            !w4_06 <- word4 (BU.unsafeIndex chunk 06)
            !w4_07 <- word4 (BU.unsafeIndex chunk 07)
            !w4_08 <- word4 (BU.unsafeIndex chunk 08)
            !w4_09 <- word4 (BU.unsafeIndex chunk 09)
            !w4_10 <- word4 (BU.unsafeIndex chunk 10)
            !w4_11 <- word4 (BU.unsafeIndex chunk 11)
            !w4_12 <- word4 (BU.unsafeIndex chunk 12)
            !w4_13 <- word4 (BU.unsafeIndex chunk 13)
            !w4_14 <- word4 (BU.unsafeIndex chunk 14)
            !w4_15 <- word4 (BU.unsafeIndex chunk 15)

            let !w64 = fi w4_00 `B.shiftL` 60
                   .|. fi w4_01 `B.shiftL` 56
                   .|. fi w4_02 `B.shiftL` 52
                   .|. fi w4_03 `B.shiftL` 48
                   .|. fi w4_04 `B.shiftL` 44
                   .|. fi w4_05 `B.shiftL` 40
                   .|. fi w4_06 `B.shiftL` 36
                   .|. fi w4_07 `B.shiftL` 32
                   .|. fi w4_08 `B.shiftL` 28
                   .|. fi w4_09 `B.shiftL` 24
                   .|. fi w4_10 `B.shiftL` 20
                   .|. fi w4_11 `B.shiftL` 16
                   .|. fi w4_12 `B.shiftL` 12
                   .|. fi w4_13 `B.shiftL` 08
                   .|. fi w4_14 `B.shiftL` 04
                   .|. fi w4_15

            go64 (acc <> BSB.word64BE w64) etc

    go32 acc b = case BS.splitAt 8 b of
      (chunk, etc)
        | BS.null chunk -> pure acc
        | otherwise -> do
            !w4_00 <- word4 (BU.unsafeIndex chunk 00)
            !w4_01 <- word4 (BU.unsafeIndex chunk 01)
            !w4_02 <- word4 (BU.unsafeIndex chunk 02)
            !w4_03 <- word4 (BU.unsafeIndex chunk 03)
            !w4_04 <- word4 (BU.unsafeIndex chunk 04)
            !w4_05 <- word4 (BU.unsafeIndex chunk 05)
            !w4_06 <- word4 (BU.unsafeIndex chunk 06)
            !w4_07 <- word4 (BU.unsafeIndex chunk 07)

            let !w32 = fi w4_00 `B.shiftL` 28
                   .|. fi w4_01 `B.shiftL` 24
                   .|. fi w4_02 `B.shiftL` 20
                   .|. fi w4_03 `B.shiftL` 16
                   .|. fi w4_04 `B.shiftL` 12
                   .|. fi w4_05 `B.shiftL` 08
                   .|. fi w4_06 `B.shiftL` 04
                   .|. fi w4_07

            go32 (acc <> BSB.word32BE w32) etc

    go16 acc b = case BS.splitAt 4 b of
      (chunk, etc)
        | BS.null chunk -> pure acc
        | otherwise -> do
            !w4_00 <- word4 (BU.unsafeIndex chunk 00)
            !w4_01 <- word4 (BU.unsafeIndex chunk 01)
            !w4_02 <- word4 (BU.unsafeIndex chunk 02)
            !w4_03 <- word4 (BU.unsafeIndex chunk 03)

            let !w16 = fi w4_00 `B.shiftL` 12
                   .|. fi w4_01 `B.shiftL` 08
                   .|. fi w4_02 `B.shiftL` 04
                   .|. fi w4_03

            go16 (acc <> BSB.word16BE w16) etc

    go8 acc b  = case BS.splitAt 2 b of
      (chunk, etc)
        | BS.null chunk -> pure acc
        | otherwise -> do
            !w4_00 <- word4 (BU.unsafeIndex chunk 00)
            !w4_01 <- word4 (BU.unsafeIndex chunk 01)

            let !w8 = fi w4_00 `B.shiftL` 04
                  .|. fi w4_01

            go8 (acc <> BSB.word8 w8) etc

