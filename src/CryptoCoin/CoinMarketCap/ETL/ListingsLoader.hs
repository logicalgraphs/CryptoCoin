{-# LANGUAGE OverloadedStrings #-}

module CryptoCoin.CoinMarketCap.ETL.ListingsLoader where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.Types

import qualified Data.ByteString.Char8 as B

import qualified Data.Map as Map

import Data.Time (Day)

import Data.CryptoCurrency.Types
import CryptoCoin.CoinMarketCap.Types

import Store.SQL.Util.Indexed hiding (idx)

data Ranking =
   Ranking { fileId :: Integer, forDay :: Day, 
             coinId :: Integer, place :: Integer }
      deriving (Eq, Ord, Show)

toRank :: IxValue MetaData -> [Ranking]
toRank (IxV i (MetaData (Status d _ _ _ _ _) ecoins)) =
   map (Ranking i d . fi . idx <*> fi . rank) (Map.elems ecoins)
      where fi = fromIntegral

instance ToRow Ranking where
   toRow (Ranking srcId d coinId r) =
      [toField coinId, toField d, toField srcId, toField r]

rankInsertQuery :: Query
rankInsertQuery = Query . B.pack $ unwords [
   "INSERT INTO coin_market_cap_daily_ranking",
   "(cmc_id, date, rank_src_id, rank) VALUES (?, ?, ?, ?)"]

insertRankings :: Connection -> IxValue MetaData -> IO ()
insertRankings conn md =
   let rnks = toRank md in
   putStrLn (unwords ["Inserting", show (length rnks), "rankings for",
                      show (date (val md))])             >>
   executeMany conn rankInsertQuery (toRank md)    >>
   putStrLn "... done."
