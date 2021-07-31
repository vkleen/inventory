{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}
module PartNumbers where

import Data.Function ( (&) )

import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.ByteString.Builder qualified as B
import Data.ByteString.Lazy qualified as BL
import Data.ByteArray ( convert )

import Crypto.Hash ( hashWith, Blake2b_512(..) )

import Inventory.Types ( MfgPN(..), PN(..) )

createPN :: MfgPN -> PN
createPN (MfgPN x) = PN $
  "P" <> ( T.encodeUtf8 x
         & hashWith Blake2b_512
         & convert
         & B.byteStringHex
         & B.toLazyByteString
         & BL.toStrict
         & T.decodeUtf8
         & T.toUpper
         & T.take 23
         )
