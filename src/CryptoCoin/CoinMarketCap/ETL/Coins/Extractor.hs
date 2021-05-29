{-# LANGUAGE OverloadedStrings #-}

module CryptoCoin.CoinMarketCap.ETL.Coins.Extractor where

-- we derive new coins from coins not already in the database

import Data.List (partition)
import qualified Data.Map as Map

import Database.PostgreSQL.Simple

import CryptoCoin.CoinMarketCap.Types
import Data.CryptoCurrency.Types.Coin (allCoinsLk)

newCoins :: Connection -> MetaData -> IO NewCoins
newCoins conn (MetaData _ m) =
   partition (not . isToken)
   . map coin
   . Map.elems
   . foldr Map.delete m
   . Map.elems
   <$> allCoinsLk conn
