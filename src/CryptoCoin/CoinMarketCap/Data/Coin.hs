{-# LANGUAGE OverloadedStrings #-}

module CryptoCoin.CoinMarketCap.Data.Coin where

import Database.PostgreSQL.Simple (Connection)

import Store.SQL.Connection (withConnection, Database(ECOIN))

-- let's get all the coins as a LookupTable

import Store.SQL.Util.LookupTable (lookupTableFrom)

import Data.LookupTable (LookupTable)

allCoinsLk :: Connection -> IO LookupTable
allCoinsLk conn = lookupTableFrom conn "SELECT cmc_id,symbol FROM coin"

{--
>>> import Store.SQL.Connection 
>>> import qualified Data.Map as Map
>>> withConnection ECOIN (\conn -> allCoinsLk conn >>= mapM_ print . take 5 . Map.toList)
("$ANRX",8057)
("$BASED",6570)
("$COIN",7796)
("$KING",7569)
("$NOOB",7646)
--}
