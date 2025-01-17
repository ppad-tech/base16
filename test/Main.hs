{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

module Main where

import qualified "ppad-base16" Data.ByteString.Base16 as B16
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ testGroup "base16" [ tests ]

pec = "6a746f62696e2077617320686572652062656e6368696e67207374756666"
inp = "jtobin was here benching stuff"

tests = testGroup "base16" [
    testCase "encode"  $ assertEqual mempty pec (B16.encode  inp)
  ]

