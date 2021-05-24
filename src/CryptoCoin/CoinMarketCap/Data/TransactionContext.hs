{-# LANGUAGE OverloadedStrings #-}

module CryptoCoin.CoinMarketCap.Data.TransactionContext where

import Database.PostgreSQL.Simple (Connection)

import CryptoCoin.CoinMarketCap.Data.Coin (allCoinsLk)
import CryptoCoin.CoinMarketCap.Data.Portfolio (portfoliiLk)

import Data.LookupTable

import Store.SQL.Util.LookupTable

data TransactionContext = TC { symLk, callLk, portfolioLk :: LookupTable }
   deriving (Eq, Ord, Show)

transContext :: Connection -> IO TransactionContext
transContext conn =
   TC <$> allCoinsLk conn <*> lookupTable conn "call_lk" <*> portfoliiLk conn
