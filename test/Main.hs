{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

module Main where

import Control.Monad (forM_)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified "ppad-base16" Data.ByteString.Base16 as B16
import qualified "ppad-base16" Data.ByteString.Base16.Pure as Pure
import qualified "base16-bytestring" Data.ByteString.Base16 as R0
import Data.Word (Word8)
import Test.Tasty
import qualified Test.Tasty.QuickCheck as Q
import qualified Test.Tasty.HUnit as H

-- generators -----------------------------------------------------------------

-- random bytes, as a slice at a random offset into a larger buffer, so
-- that inputs with a non-zero 'ByteString' offset are exercised
bytes :: Int -> Q.Gen BS.ByteString
bytes k = do
  l <- Q.chooseInt (0, k)
  o <- Q.chooseInt (0, 32)
  v <- Q.vectorOf (o + l) Q.arbitrary
  pure (BS.drop o (BS.pack v))

newtype BS = BS BS.ByteString
  deriving (Eq, Show)

instance Q.Arbitrary BS where
  arbitrary = do
    b <- bytes 1024
    pure (BS b)

hex_chars :: BS.ByteString
hex_chars = "0123456789abcdefABCDEF"

-- mostly-hex strings of arbitrary (including odd) length: each byte is
-- a hex char with high probability, otherwise an arbitrary byte
newtype Hexish = Hexish BS.ByteString
  deriving (Eq, Show)

instance Q.Arbitrary Hexish where
  arbitrary = do
    l <- Q.chooseInt (0, 256)
    o <- Q.chooseInt (0, 32)
    junk <- Q.elements [0, 1, 50 :: Int]
    v <- Q.vectorOf (o + l) $ do
      p <- Q.chooseInt (1, 100)
      if   p <= junk
      then Q.arbitrary
      else Q.elements (BS.unpack hex_chars)
    pure (Hexish (BS.drop o (BS.pack v)))

-- decoders under test --------------------------------------------------------

decoders :: [(String, BS.ByteString -> Maybe BS.ByteString)]
decoders = [("public", B16.decode), ("pure", Pure.decode)]

ref_decode :: BS.ByteString -> Maybe BS.ByteString
ref_decode bs = case R0.decode bs of
  Left _  -> Nothing
  Right d -> Just d

is_hex :: Word8 -> Bool
is_hex c = BS.elem c hex_chars

-- properties -----------------------------------------------------------------

encode_matches_reference :: BS -> Bool
encode_matches_reference (BS bs) =
  let r0 = R0.encode bs
  in  B16.encode bs == r0 && Pure.encode bs == r0

decode_inverts_encode :: BS -> Bool
decode_inverts_encode (BS bs) =
  let enc = B16.encode bs
  in  all (\(_, dec) -> dec enc == Just bs) decoders

decode_matches_reference :: Hexish -> Bool
decode_matches_reference (Hexish bs) =
  let r0 = ref_decode bs
  in  B16.decode bs == r0 && Pure.decode bs == r0

-- unit tests -----------------------------------------------------------------

case_handled :: TestTree
case_handled = H.testCase "decodes uppercase and mixed-case hex" $
  forM_ decoders $ \(nam, dec) -> do
    let l = dec "deadbeef"
    H.assertEqual nam (Just "\xde\xad\xbe\xef") l
    H.assertEqual nam l (dec "DEADBEEF")
    H.assertEqual nam l (dec "DeAdBeEf")

-- every byte value, at every position of a string spanning one full NEON
-- block plus a scalar tail, is accepted iff it is a hex char
every_byte_every_position :: TestTree
every_byte_every_position =
  H.testCase "every byte at every position" $ do
    let base = B8.replicate 34 '0'
    forM_ [0 .. 255 :: Int] $ \c ->
      forM_ [0 .. BS.length base - 1] $ \i -> do
        let w   = fromIntegral c :: Word8
            inp = BS.take i base <> BS.singleton w <> BS.drop (i + 1) base
            pec = if is_hex w then ref_decode inp else Nothing
            msg = "byte " <> show c <> ", position " <> show i
        forM_ decoders $ \(nam, dec) ->
          H.assertEqual (nam <> ", " <> msg) pec (dec inp)

-- for every length spanning several NEON blocks and every tail length,
-- a single invalid byte at any position causes decoding to fail
single_corruption :: TestTree
single_corruption = H.testCase "single invalid byte anywhere" $ do
  let bad = BS.pack [0x00, 0x20, 0x7f, 0x80, 0xff] <> "/:@G`g"
  forM_ [1 .. 80 :: Int] $ \n -> do
    let raw = BS.pack (fmap fromIntegral [7 * j + 3 | j <- [1 .. n]])
        enc = upper_odd (B16.encode raw)
        upper_odd = BS.pack . zipWith up [0 :: Int ..] . BS.unpack
        up j c | odd j && c >= 0x61 = c - 0x20
               | otherwise          = c
    forM_ decoders $ \(nam, dec) ->
      H.assertEqual (nam <> ", intact, n = " <> show n) (Just raw) (dec enc)
    forM_ [0 .. BS.length enc - 1] $ \i ->
      forM_ (BS.unpack bad) $ \w -> do
        let inp = BS.take i enc <> BS.singleton w <> BS.drop (i + 1) enc
            msg = "n = " <> show n <> ", position " <> show i
                  <> ", byte " <> show w
        forM_ decoders $ \(nam, dec) ->
          H.assertEqual (nam <> ", " <> msg) Nothing (dec inp)

odd_lengths :: TestTree
odd_lengths = H.testCase "odd-length inputs" $
  forM_ [1, 3 .. 99 :: Int] $ \l ->
    forM_ decoders $ \(nam, dec) ->
      H.assertEqual (nam <> ", length " <> show l) Nothing
        (dec (B8.replicate l 'a'))

main :: IO ()
main = defaultMain $
  testGroup "ppad-base16" [
    testGroup "property tests" [
      Q.testProperty "encode matches reference" $
        Q.withMaxSuccess 5000 encode_matches_reference
    , Q.testProperty "decode . encode ~ id" $
        Q.withMaxSuccess 5000 decode_inverts_encode
    , Q.testProperty "decode matches reference" $
        Q.withMaxSuccess 5000 decode_matches_reference
    ]
  , testGroup "unit tests" [
      case_handled
    , every_byte_every_position
    , single_corruption
    , odd_lengths
    ]
  ]
