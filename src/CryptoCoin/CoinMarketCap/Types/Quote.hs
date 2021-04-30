{-# LANGUAGE OverloadedStrings #-}

module CryptoCoin.CoinMarketCap.Types.Quote where

import Data.Aeson

import Database.PostgreSQL.Simple.FromRow

data Quote = Quote {
   price, volume24h                                     :: Double,
   percentChange1h, percentChange24h, percentChange7d   :: Maybe Double,
   percentChange30d, percentChange60d, percentChange90d :: Maybe Double,
   marketCap                                            :: Double }
      deriving (Eq, Ord, Show) 

instance FromJSON Quote where
   parseJSON = withObject "quote" $ \v ->
      Quote <$> v .:  "price"              <*> v .:  "volume_24h"
            <*> v .:? "percent_change_1h"  <*> v .:? "percent_change_24h"
            <*> v .:? "percent_change_7d"  <*> v .:? "percent_change_30d"
            <*> v .:? "percent_change_60d" <*> v .:? "percent_change_90d"
            <*> v .:  "market_cap"

instance FromRow Quote where
   fromRow = Quote <$> field <*> field <*> field <*> field <*> field
                   <*> field <*> field <*> field <*> field
