module Data.CryptoCurrency.Types.Transfers.Context where

import Database.PostgreSQL.Simple (Connection)

import Data.CryptoCurrency.Types.Portfolio (fetchPortfolii, Portfolii)

import Data.LookupTable

import Store.SQL.Util.LookupTable (lookupTable)

-- TRANSFER CONTEXT ---------------------------------------------------

data TransferContext =
   TfC { dirLk :: LookupTable, portfolii :: Portfolii }
      deriving (Eq, Ord, Show)

transFContext :: Connection -> IO TransferContext
transFContext conn =
   TfC <$> lookupTable conn "transfer_direction_lk" <*> fetchPortfolii conn
