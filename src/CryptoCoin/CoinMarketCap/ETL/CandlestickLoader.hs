{-# LANGUAGE OverloadedStrings #-}

module CrytoCoin.CoinMarketCap.ETL.CandlestickLoader where

{-- 
We grab the candlesticks for the tracked coins.

This means we need to

1. grab the tracked coins (symbols and cmc_ids)
2. grab the max-date of the candlesticks for each symbol
2. a. if the max date is yesterday, we do nothing (remove the coin from the
      list), because we good.
2. b. (or not 2. b.) if there is no max(for_date), we set it to tday -30
3. for each of the (remaining) coins, we fetch the candlesticks from the
   utcTimeToPOSIXSeconds max-date to yesterday
4. we upload each of those 'files' (marked with the cmc_id) to the source table,
   marked as unprocessed.

The url is of this form:

https://query1.finance.yahoo.com/v7/finance/download/BTC-USD?period1=1616711765&period2=1616798165&interval=1d&events=history&includeAdjustedClose=true

--}

import qualified Data.ByteString.Char8 as B
import Data.Time.TimeSeries (today)

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types

-- let's get the tracked coins as a LookupTable

import Store.SQL.Connection (withConnection, Database(ECOIN))
import Store.SQL.Util.LookupTable

import Data.LookupTable (LookupTable)

trackedCoinsQuery :: Query
trackedCoinsQuery = Query . B.pack $ unwords [
   "SELECT cmc_id,symbol FROM coin",
   "WHERE cmc_id IN (SELECT DISTINCT tracked_coin_id FROM",
   "j_tracked_coin_tracked_type)"]

trackedCoins :: Connection -> IO LookupTable
trackedCoins conn = lookupTableFrom conn trackedCoinsQuery

{--
>>> withConnection ECOIN (\conn -> trackedCoins conn >>= mapM_ print . Map.toList)
("AAVE",7278)
("ADA",2010)
("ALGO",4030)
("ANKR",3783)
("ATOM",3794)
("BAL",5728)
("BAND",4679)
("BAT",1697)
("BCH",1831)
("BTC",1)
...
("XTZ",2011)
("YFI",5864)
("ZEC",1437)
("ZIL",2469)
--}

-- for each of those coins, we need that max(for_date):
