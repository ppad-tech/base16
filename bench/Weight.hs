{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

module Main where

import qualified Data.ByteString as BS
import qualified "base16-bytestring" Data.ByteString.Base16 as R0
import qualified "base16" Data.ByteString.Base16 as R1
import qualified "ppad-base16" Data.ByteString.Base16 as B16
import qualified Weigh as W

inp :: BS.ByteString
inp = "jtobin was here benching stuffjtobin was here benching stufjtobin was here benching stufjtobin was here benching stufjtobin was here benching stufjtobin was here benching stufjtobin was here benching stufjtobin was here benching stufjtobin was here benching stufjtobin was here benching stufjtobin was here benching stufjtobin was here benching stufjtobin was here benching stufjtobin was here benching stufjtobin was here benching stufjtobin was here benching stufjtobin was here benching stufjtobin was here benching stufjtobin was here benching stufjtobin was here benching stufjtobin was here benching stufjtobin was here benching stufjtobin was here benching stufjtobin was here benching stufjtobin was here benching stufjtobin was here benching stufjtobin was here benching stufjtobin was here benching stufjtobin was here benching stufjtobin was here benching stufjtobin was here benching stufjtobin was here benching stufjtobin was here benching stufjtobin was here benching stufjtobin was here benching stufjtobin w"

hinp :: BS.ByteString
hinp = B16.encode inp

main :: IO ()
main = W.mainWith $ do
  W.func "ppad-base16 (encode)" B16.encode inp
  W.func "base16-bytestring (encode)" R0.encode inp
  W.func "base16 (encode)" R1.encodeBase16' inp

  W.func "ppad-base16 (decode)" B16.decode hinp
  W.func "base16-bytestring (decode)" R0.decode hinp
  W.func "base16 (decode)" R1.decodeBase16Untyped inp
