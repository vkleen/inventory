module Inventory.Types where

import qualified Data.Text as T

newtype MfgPN = MfgPN T.Text
  deriving (Show)

newtype PN = PN T.Text
  deriving (Show, Eq, Ord)
