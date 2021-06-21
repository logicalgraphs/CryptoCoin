{-# LANGUAGE OverloadedStrings #-}

module Data.CryptoCurrency.Types.Transfers.Internal where

import Control.Monad (void)

import qualified Data.ByteString.Char8 as B
import Data.Time

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.Types (Query(Query))

import Data.CryptoCurrency.Types
import Data.LookupTable
import Data.Monetary.USD

----- Handling Cash transfers ------------------------------------------------

data CashTransF' = Cshxf Day USD Idx Idx
   deriving (Eq, Ord, Show)

instance ToRow CashTransF' where
   toRow (Cshxf d a p dir) = [toField d, toField p, toField dir, toField a]

storeCashTransFQuery :: Query
storeCashTransFQuery = Query . B.pack $ unwords [
   "INSERT INTO transfer_funds (for_date, portfolio_id, transfer_direction_id,",
   "amount) VALUES (?, ?, ?, ?)"]

storeCashTransF' :: Connection -> [CashTransF'] -> IO ()
storeCashTransF' conn = void . executeMany conn storeCashTransFQuery

----- Now: coin transfers ---------------------------------------------------

                      --  dt  amt    coin surcharge from to
data CoinTransF' = Cnxf Day Double Idx Double Idx Idx
   deriving (Eq, Ord, Show)

instance ToRow CoinTransF' where
   toRow (Cnxf dt amt cn sur frm to) =
      [toField dt, toField amt, toField cn,
       toField sur, toField frm, toField to]

storeCoinTransFQuery :: Query
storeCoinTransFQuery = Query . B.pack $ unwords [
   "INSERT INTO transfer_coin (for_date,amount,cmc_id,surcharge,transfer_from,",
   "transfer_to) VALUES (?, ?, ?, ?, ?, ?)"]
