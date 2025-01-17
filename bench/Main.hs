{-# LANGUAGE PackageImports #-}

module Main where

import Criterion.Main
import qualified "ppad-base16" Data.ByteString.Base16 as B16
import qualified "base16-bytestring" Data.ByteString.Base16 as R0
import qualified "base16" Data.ByteString.Base16 as R1

main :: IO ()
main = defaultMain [ suite ]

base16_encode :: Benchmark
base16_encode = bgroup "base16 encode" [
    bench "120b" $ nf B16.encode "jtobin was here"
  , bench "240b" $ nf B16.encode "jtobin was herebenching stuff."
  ]

base16_encode' :: Benchmark
base16_encode' = bgroup "base16 encode" [
    bench "120b" $ nf B16.encode' "jtobin was here"
  , bench "240b" $ nf B16.encode' "jtobin was herebenching stuff."
  ]

base16_decode :: Benchmark
base16_decode = bgroup "base16 decode" [
    bench "120b" $ nf B16.decode "6a746f62696e207761732068657265"
  , bench "240b" $ nf B16.decode
      "6a746f62696e20776173206865726562656e6368696e672073747566662e"
  ]

r0_encode :: Benchmark
r0_encode = bgroup "base16 encode" [
    bench "120b" $ nf R0.encode "jtobin was here"
  , bench "240b" $ nf R0.encode "jtobin was herebenching stuff."
  ]

r0_decode :: Benchmark
r0_decode = bgroup "base16 decode" [
    bench "120b" $ nf R0.decode "6a746f62696e207761732068657265"
  , bench "240b" $ nf R0.decode
      "6a746f62696e20776173206865726562656e6368696e672073747566662e"
  ]

r1_encode :: Benchmark
r1_encode = bgroup "base16 encode" [
    bench "120b" $ nf R1.encodeBase16' "jtobin was here"
  , bench "240b" $ nf R1.encodeBase16' "jtobin was herebenching stuff."
  ]

r1_decode :: Benchmark
r1_decode = bgroup "base16 decode" [
    bench "120b" $ nf R1.decodeBase16Untyped "6a746f62696e207761732068657265"
  , bench "240b" $ nf R1.decodeBase16Untyped
      "6a746f62696e20776173206865726562656e6368696e672073747566662e"
  ]

suite :: Benchmark
suite = bgroup "benchmarks" [
      bgroup "ppad-base16" [
          base16_encode
        , base16_encode'
        , base16_decode
    --  ]
    --, bgroup "base16-bytestring" [
    --      r0_encode
    --    , r0_decode
    --  ]
    --, bgroup "base16" [
    --      r1_encode
    --    , r1_decode
      ]
    ]

