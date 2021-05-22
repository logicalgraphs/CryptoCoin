{-# LANGUAGE OverloadedStrings #-}

module CryptoCoin.CoinMarketCap.Data.Portfolio where

import Database.PostgreSQL.Simple (Connection)

import Store.SQL.Connection (withConnection, Database(ECOIN))

-- let's get the portfolii as a LookupTable

import Store.SQL.Util.LookupTable (lookupTableFrom)

import Data.LookupTable (LookupTable)

portfoliiLk :: Connection -> IO LookupTable
portfoliiLk conn =
   lookupTableFrom conn "SELECT portfolio_id, portfolio_name FROM portfolio"

{--
>>> import Store.SQL.Connection 
>>> import qualified Data.Map as Map
>>> withConnection ECOIN (\conn -> portfoliiLk conn >>= mapM_ print . Map.toList)
("BINANCE",2)
("COINBASE",1)
("GEMINI",3)
--}
