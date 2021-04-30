{-# LANGUAGE OverloadedStrings #-}

module CryptoCoin.CoinMarketCap.ETL.Coins.NewCoins where

-- we derive new coins from coins not already in the database

import Data.List (partition)
import qualified Data.Map as Map

import Database.PostgreSQL.Simple

import CryptoCoin.CoinMarketCap.Types

import Store.SQL.Util.Indexed (Index, IxValue(IxV), idx)

newCoins :: Connection -> MetaData -> IO NewCoins
newCoins conn (MetaData _ m) =
   partition (not . isToken)
   . map coin
   . Map.elems
   . foldr Map.delete m
   . map idx
   <$> coins conn

-- to do that, we need to extract the indices from the database, ... with
-- (any other) value

coins :: Connection -> IO [Index]
coins conn = query_ conn "SELECT cmc_id FROM coin"
