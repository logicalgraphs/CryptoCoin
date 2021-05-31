module Data.CryptoCurrency.Types.Transactions.Context where

import Database.PostgreSQL.Simple (Connection)

import Data.CryptoCurrency.Types.Coin (allCoinsLk, CoinIdsLookup)
import Data.CryptoCurrency.Types.Portfolio (portfoliiLk)

import Data.LookupTable

import Store.SQL.Util.LookupTable (lookupTable)

-- TRANSACTION CONTEXT ---------------------------------------------------

data TransactionContext =
   TC { symLk :: CoinIdsLookup, callLk, portfolioLk :: LookupTable }
      deriving (Eq, Ord, Show)

transContext :: Connection -> IO TransactionContext
transContext conn =
   TC <$> allCoinsLk conn <*> lookupTable conn "call_lk" <*> portfoliiLk conn
