{-# LANGUAGE OverloadedStrings #-}

module Data.CryptoCurrency.Types.Transfers.Internal where

import Control.Monad (void)

import qualified Data.ByteString.Char8 as B
import Data.Time (Day)

import Database.PostgreSQL.Simple (Connection, executeMany, query_, FromRow)
import Database.PostgreSQL.Simple.FromRow (fromRow, field)
import Database.PostgreSQL.Simple.ToField (toField)
import Database.PostgreSQL.Simple.ToRow (ToRow, toRow)
import Database.PostgreSQL.Simple.Types (Query(Query))

import Data.CryptoCurrency.Types (Idx)
import Data.CryptoCurrency.Utils (plural)

import Data.Monetary.USD (USD)

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

msg :: Int -> String
msg su | su == 0 = "Transfering no cash today."
       | otherwise = "Storing " ++ show su ++ " transfer" ++ plural su

----- Now: coin transfers ---------------------------------------------------

                      --  dt  amt    coin surcharge basis from to
data CoinTransF' = Cnxf Day Double Idx Double USD Idx Idx
   deriving (Eq, Ord, Show)

storeCoinTransFQuery :: Query
storeCoinTransFQuery = Query . B.pack $ unwords [
   "INSERT INTO transfer_coin (for_date,amount,cmc_id,surcharge,transfer_from,",
   "transfer_to,cost_basis) VALUES (?, ?, ?, ?, ?, ?, ?)"]

instance ToRow CoinTransF' where
   toRow (Cnxf dt amt cn sur bas frm to) =
      [toField dt, toField amt, toField cn,
       toField sur, toField frm, toField to, toField bas]

storeCnXs :: Connection -> [CoinTransF'] -> IO ()
storeCnXs conn = void . executeMany conn storeCoinTransFQuery

fetchAllCoinTransFQuery :: Query
fetchAllCoinTransFQuery = Query . B.pack $ unwords [
   "SELECT for_date,amount,cmc_id,surcharge,cost_basis,",
   "transfer_from,transfer_to",
   "FROM transfer_coin ORDER BY for_date"]

instance FromRow CoinTransF' where
   fromRow = Cnxf <$> field <*> field <*> field
                  <*> field <*> field <*> field <*> field

fetchAllCoinTransFs :: Connection -> IO [CoinTransF']
fetchAllCoinTransFs = flip query_ fetchAllCoinTransFQuery
