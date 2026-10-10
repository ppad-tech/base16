{-# OPTIONS_HADDOCK prune #-}

-- |
-- Module: Data.ByteString.Base16
-- Copyright: (c) 2025 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- Base16 encoding and decoding of strict bytestrings.

module Data.ByteString.Base16 (
    encode
  , decode
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16.Arm as Arm
import qualified Data.ByteString.Base16.Pure as Pure

-- | Encode a base256 'ByteString' as base16.
--
--   Uses ARM NEON extensions when available, otherwise a pure
--   Haskell scalar loop.
--
--   >>> encode "hello world"
--   "68656c6c6f20776f726c64"
encode :: BS.ByteString -> BS.ByteString
encode bs
  | Arm.base16_arm_available = Arm.encode bs
  | otherwise = Pure.encode bs
{-# INLINABLE encode #-}

-- | Decode a base16 'ByteString' to base256.
--
--   Uses ARM NEON extensions when available, otherwise a pure
--   Haskell scalar loop.  Invalid inputs (including odd-length
--   inputs) will produce 'Nothing'.
--
--   >>> decode "68656c6c6f20776f726c64"
--   Just "hello world"
--   >>> decode "068656c6c6f20776f726c64" -- odd-length
--   Nothing
decode :: BS.ByteString -> Maybe BS.ByteString
decode bs
  | Arm.base16_arm_available = Arm.decode bs
  | otherwise = Pure.decode bs
{-# INLINABLE decode #-}
