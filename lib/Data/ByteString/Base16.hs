{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE OverloadedStrings #-}

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
import Data.Word (Word8)

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

data W8Pair = Pair
  {-# UNPACK #-} !Word8
  {-# UNPACK #-} !Word8

hilo :: Word8 -> W8Pair
hilo b =
  let !hi = BU.unsafeIndex hex_charset (fromIntegral b `B.shiftR` 4)
      !lo = BU.unsafeIndex hex_charset (fromIntegral b .&. 0b00001111)
  in  Pair hi lo

encode :: BS.ByteString -> BS.ByteString
encode bs@(BI.PS _ _ l)
    | l < 128 = to_strict_small (go 0)
    | otherwise = to_strict (go 0)
  where
    go j
      | j == l = mempty
      | otherwise =
          let !(Pair hi lo) = hilo (BU.unsafeIndex bs j)
              w16 = fromIntegral hi `B.shiftL` 8
                .|. fromIntegral lo
          in  BSB.word16BE w16 <> go (succ j)

word4 :: Word8 -> Maybe Word8
word4 w8 = fmap fi (BS.elemIndex w8 hex_charset)

decode :: BS.ByteString -> Maybe BS.ByteString
decode b16@(BI.PS _ _ b16_l)
    | B.testBit b16_l 0 = Nothing
    | b16_l `quot` 2 < 128 = fmap to_strict_small (go mempty b16)
    | otherwise = fmap to_strict (go mempty b16)
  where
    go acc !bs@(BI.PS _ _ l)
      | l == 0 = pure $! acc
      | otherwise = case BS.splitAt 2 bs of
          (chunk, etc) -> do
            !(Pair hi lo) <- Pair
              <$> word4 (BU.unsafeIndex chunk 0)
              <*> word4 (BU.unsafeIndex chunk 1)

            let !b =  hi `B.shiftL` 4
                  .|. lo

            go (acc <> BSB.word8 b) etc

