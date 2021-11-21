module CryptoCoin.VfatTools.Scanners.Types where

import CryptoCoin.VfatTools.Types

import Control.Scan.CSV (readMaybe)

import Data.Monetary.USD

type FileScanner = [String] -> (Maybe YieldFarm, [String])

readCleenUSD :: String -> Maybe USD
readCleenUSD = readMaybe . filter (/= ',')

type LPScanner = String -> Maybe (Coin, USD)
