{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

module Main where

import qualified Data.ByteString as BS
import qualified "ppad-base16" Data.ByteString.Base16 as B16
import qualified "base16-bytestring" Data.ByteString.Base16 as R0
import Test.Tasty
import qualified Test.Tasty.QuickCheck as Q

newtype BS = BS BS.ByteString
  deriving (Eq, Show)

bytes :: Int -> Q.Gen BS.ByteString
bytes k = do
  l <- Q.chooseInt (0, k)
  v <- Q.vectorOf l Q.arbitrary
  pure (BS.pack v)

instance Q.Arbitrary BS where
  arbitrary = do
    b <- bytes 1024
    pure (BS b)

decode_inverts_encode :: BS -> Bool
decode_inverts_encode (BS bs) = case B16.decode (B16.encode bs) of
  Nothing -> False
  Just b  -> b == bs

encode_matches_reference :: BS -> Bool
encode_matches_reference (BS bs) =
  let us = B16.encode bs
      r0 = R0.encode bs
  in  us == r0

decode_matches_reference :: BS -> Bool
decode_matches_reference (BS bs) =
  let enc = R0.encode bs
      us  = B16.decode enc
      r0  = R0.decode enc
  in  case us of
        Nothing -> case r0 of
          Left _ -> True
          _ -> False
        Just du -> case r0 of
          Left _ -> False
          Right d0 -> du == d0

main :: IO ()
main = defaultMain $
  testGroup "ppad-base16" [
    testGroup "property tests" [
      Q.testProperty "decode . encode ~ id" $
        Q.withMaxSuccess 5000 decode_inverts_encode
    , Q.testProperty "encode matches reference" $
        Q.withMaxSuccess 5000 encode_matches_reference
    , Q.testProperty "decode matches reference" $
        Q.withMaxSuccess 5000 decode_matches_reference
    ]
  ]

