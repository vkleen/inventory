{-# LANGUAGE OverloadedStrings #-}
module PartNumbers where

import Data.Function ((&))

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BL
import Data.ByteArray (convert)

import Crypto.Hash

import Inventory.Types

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
