module Data.CryptoCurrency.Types.Transfers.Context where

import Database.PostgreSQL.Simple (Connection)

import CryptoCoin.CoinMarketCap.Data.TrackedCoin (trackedCoins)

import Data.CryptoCurrency.Types.Portfolio
           (fetchPortfolii, Portfolii, portfoliiLk)
import Data.LookupTable (LookupTable)

import Store.SQL.Util.LookupTable (lookupTable)

-- TRANSFER Funds CONTEXT ---------------------------------------------------

data TransferFundsContext =
   TfC { dirLk :: LookupTable, portfolii :: Portfolii }
      deriving (Eq, Ord, Show)

transFContext :: Connection -> IO TransferFundsContext
transFContext conn =
   TfC <$> lookupTable conn "transfer_direction_lk" <*> fetchPortfolii conn

-- TRANSFER Coins CONTEXT ---------------------------------------------------

data TransferCoinContext =  TcC { cnLk, portLk :: LookupTable }
   deriving (Eq, Ord, Show)

transCContext :: Connection -> IO TransferCoinContext
transCContext conn = TcC <$> trackedCoins conn <*> portfoliiLk conn
