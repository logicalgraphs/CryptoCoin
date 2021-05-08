{-# LANGUAGE OverloadedStrings #-}

module Data.CryptoCurrency.Types.Transactions.Internal where

{--
A transaction HAS_A

   call
   portfolio (id) (that we get from the portfolio name and portfolio map)
     (so we need a portfolio map)
   coin id we get from the coin symbol

   the rest we get from the transaction details

Now we need the transaction Id to associate to the recommendations for the
coin at this date bought or sold in this transaction.
--}

import qualified Data.ByteString.Char8 as B

import Data.Foldable (toList)
import Data.Time (Day)

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.Types

import Data.CryptoCurrency.Types (IxRow(IxRow))
import Data.CryptoCurrency.Types.Recommendation
import Data.Monetary.USD

import Store.SQL.Connection (withConnection, Database(ECOIN))
import Store.SQL.Util.Indexed (Index(Idx), IxValue, idx, val)
import Store.SQL.Util.Pivots (Pivot(Pvt), joinValue)

data Trans' = Trans' Day USD USD Double Integer Integer Integer 
   deriving (Eq, Ord, Show)

-- STORE-SIDE -------------------------------------------------------

instance ToRow Trans' where
   toRow (Trans' forDate amount surcharge coins callId portfolioId coinId) =
      [toField coinId, toField forDate, toField amount, toField surcharge,
       toField coins, toField callId, toField portfolioId]

storeTransQuery :: Query
storeTransQuery = Query . B.pack $ unlines [
   "INSERT INTO transaction_log (cmc_id, for_date, purchase_usd,",
   "surcharge_usd, n_coins, call_id, portfolio_id)",
   "VALUES (?, ?, ?, ?, ?, ?, ?) returning transaction_id"]

storeTrans' :: Connection -> Trans' -> IO Index
storeTrans' conn trans = head <$> query conn storeTransQuery trans

type CoinDayTransaction = IxRow Integer

realize :: Connection -> Trans' -> IO CoinDayTransaction
realize conn t@(Trans' d _ _ _ _ _ cix) =
   storeTrans' conn t >>= \(Idx i) -> return (IxRow cix d i)

fetchCoinRecsQuery :: Query
fetchCoinRecsQuery = Query . B.pack $ unlines [
   "SELECT recommendation_id, cmc_id FROM recommendation",
   "WHERE for_date IN ? AND cmc_id IN ?"]

type CoinRc' = IxValue Index

fetchCoinRecs' :: Foldable t => Connection -> t Day -> t Integer -> IO [CoinRc']
fetchCoinRecs' conn days coins =
   query conn fetchCoinRecsQuery (oitl days, oitl coins)
      where oitl = In . toList

type CoinRec = Pivot

fetchCoinRecs :: Foldable t => Connection -> t Day -> t Integer -> IO [CoinRec]
fetchCoinRecs conn days coins =
   map (Pvt . idx <*> (idx . val)) <$> fetchCoinRecs' conn days coins

-- FETCH-SIDE -------------------------------------------------------

instance FromRow Trans' where
   fromRow = Trans' <$> field <*> field <*> field <*> field
                    <*> field <*> field <*> field

fetchTransQuery :: String -> Query
fetchTransQuery whereClause = Query . B.pack $ unlines [
   "SELECT cmc_id, for_date, purchase_usd, surcharge_usd, n_coins, call_id,",
   "portfolio_id FROM transaction_log", whereClause]

fetchTrans :: Connection -> String -> IO [Trans']
fetchTrans conn = query_ conn . fetchTransQuery
