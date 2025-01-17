{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

module Main where

import Criterion.Main
import qualified Data.ByteString as BS
import qualified "ppad-base16" Data.ByteString.Base16 as B16
import qualified "base16-bytestring" Data.ByteString.Base16 as R0
import qualified "base16" Data.ByteString.Base16 as R1

main :: IO ()
main = defaultMain [
    encode
  , decode
  ]

encode :: Benchmark
encode = bgroup "encode" [
    bench "ppad-base16" $ nf B16.encode (BS.replicate 1024 0x00)
  , bench "base16-bytestring" $ nf R0.encode (BS.replicate 1024 0x00)
  , bench "base16" $ nf R1.encodeBase16' (BS.replicate 1024 0x00)
  ]

decode :: Benchmark
decode = bgroup "decode" [
    bench "ppad-base16" $ nf B16.decode
      (B16.encode (BS.replicate 512 0x00))
  , bench "base16-bytestring" $ nf R0.decode
      (B16.encode (BS.replicate 512 0x00))
  , bench "base16" $ nf R1.decodeBase16Untyped
      (B16.encode (BS.replicate 512 0x00))
  ]

decode_various :: Benchmark
decode_various = bgroup "base16" [
    bench "1024B input" $ nf B16.decode (B16.encode (BS.replicate 512 0x00))
  , bench "1026B input" $ nf B16.decode (B16.encode (BS.replicate 513 0x00))
  , bench "1028B input" $ nf B16.decode (B16.encode (BS.replicate 514 0x00))
  , bench "1030B input" $ nf B16.decode (B16.encode (BS.replicate 515 0x00))
  , bench "1032B input" $ nf B16.decode (B16.encode (BS.replicate 516 0x00))
  , bench "1034B input" $ nf B16.decode (B16.encode (BS.replicate 517 0x00))
  , bench "1036B input" $ nf B16.decode (B16.encode (BS.replicate 518 0x00))
  , bench "1038B input" $ nf B16.decode (B16.encode (BS.replicate 519 0x00))
  , bench "1040B input" $ nf B16.decode (B16.encode (BS.replicate 520 0x00))
  ]

encode_various :: Benchmark
encode_various = bgroup "base16" [
    bench "1024B input" $ nf B16.encode (BS.replicate 1024 0x00)
  , bench "1023B input" $ nf B16.encode (BS.replicate 1023 0x00)
  , bench "1022B input" $ nf B16.encode (BS.replicate 1022 0x00)
  ]
