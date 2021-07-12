{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DerivingStrategies #-}
module Scripts.ValidatePNs where

import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)
import Data.Default (def)
import Data.Function ((&))
import Data.Foldable (traverse_)

import System.Exit (exitFailure, exitSuccess)

import Hledger.Read
import Hledger.Data
import qualified Data.List as LL
import qualified Data.Text as T
import qualified Data.Map as M

import Data.Validation

import Inventory.Types
import PartNumbers

data CommodityError = NoMPNTag CommoditySymbol
                    | HashMismatch CommoditySymbol PN
  deriving stock (Show)

checkCommodity :: Commodity -> Validation [CommodityError] ()
checkCommodity x = case MfgPN <$> LL.lookup "m-pn" (ctags x) of
  Nothing -> Failure . pure $ NoMPNTag (csymbol x)
  Just mpn -> case createPN mpn of
    pn@(PN pnText) | pnText /= csymbol x -> Failure . pure $ HashMismatch (csymbol x) pn
                   | otherwise -> Success ()

checkCommodities :: M.Map k Commodity -> Validation [CommodityError] ()
checkCommodities = traverse_ checkCommodity

validatePNs :: IO ()
validatePNs = do
  journalFile <- getArgs >>= \case
    (_:journalFile:_) -> pure journalFile
    _ -> hPutStrLn stderr "validatePNs <journal>" >> exitFailure
  j <- readJournalFile (def {strict_ = True}) journalFile >>= \case
    Left e -> putStrLn e >> exitFailure
    Right j -> pure j
  jcommodities j & checkCommodities
                 & validation (\e -> traverse_ print e *> exitFailure)
                              (const exitSuccess)
