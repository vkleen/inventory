{-# LANGUAGE ImportQualifiedPost #-}
module Inventory.Types where

import Data.Text qualified as T

newtype MfgPN = MfgPN T.Text
  deriving (Show)

newtype PN = PN T.Text
  deriving (Show, Eq, Ord)
