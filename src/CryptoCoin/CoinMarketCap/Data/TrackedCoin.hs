{-# LANGUAGE OverloadedStrings #-}

module CryptoCoin.CoinMarketCap.Data.TrackedCoin where

import qualified Data.ByteString.Char8 as B

import Database.PostgreSQL.Simple (Connection)
import Database.PostgreSQL.Simple.Types

import Store.SQL.Connection (withConnection, Database(ECOIN))

-- let's get the tracked coins as a LookupTable

import Store.SQL.Util.LookupTable (lookupTableFrom)

import Data.LookupTable (LookupTable)

trackedCoinsQuery :: Query
trackedCoinsQuery = Query . B.pack $ unwords [
   "SELECT cmc_id,symbol FROM coin",
   "WHERE cmc_id IN (SELECT DISTINCT tracked_coin_id FROM",
   "j_tracked_coin_tracked_type)"]

trackedCoins :: Connection -> IO LookupTable
trackedCoins conn = lookupTableFrom conn trackedCoinsQuery

{--
>>> withConnection ECOIN (\conn -> trackedCoins conn >>=
                                   mapM_ print . Map.toList)
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

