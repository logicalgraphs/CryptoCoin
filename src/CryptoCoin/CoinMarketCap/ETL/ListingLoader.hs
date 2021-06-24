{-# LANGUAGE OverloadedStrings #-}

module CryptoCoin.CoinMarketCap.ETL.ListingLoader where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.Types

import qualified Data.ByteString.Char8 as B

import qualified Data.Map as Map

import Data.Maybe (mapMaybe)

import Data.Time (Day)

import Data.CryptoCurrency.Types hiding (price)

import CryptoCoin.CoinMarketCap.Types
import CryptoCoin.CoinMarketCap.Types.Quote

import Store.SQL.Util.Indexed hiding (idx)

data DayListing =
   DayListing { fileId :: Integer, forDay :: Day, cmcId :: Integer,
                rank1 :: Integer, numPairs :: Maybe Integer,
                sups :: Supplies, quot :: Quote }
      deriving (Eq, Ord, Show)

toListings :: IxValue MetaData -> [DayListing]
toListings (IxV i (MetaData (Status d _ _ _ _ _) listings)) =
   mapMaybe (\l@(Listing _ nmp s _tgs mbq) ->
               DayListing i d (idx l) (rank l) nmp s <$> mbq)
            (Map.elems listings)

instance ToRow DayListing where
   toRow (DayListing srcId d idx rank nmp s q) =
      [toField idx, toField nmp, toField $ maxSupply s,
       toField $ circulatingSupply s, toField $ totalSupply s, 
       toField $ price q, toField $ volume24h q,
       toField $ percentChange1h q, toField $ percentChange24h q,
       toField $ percentChange7d q, toField $ percentChange30d q,
       toField $ percentChange60d q, toField $ percentChange90d q,
       toField $ marketCap q, toField srcId, toField d, toField rank]

listingInsertQuery :: Query
listingInsertQuery = Query . B.pack $ unwords [
   "INSERT INTO coin_market_cap_daily_listing",
   "(cmc_id, num_pairs, max_supply, circulating_supply, total_supply,",
   "quote_price, volume_24h, percent_change_1h, percent_change_24h,",
   "percent_change_7d, percent_change_30d, percent_change_60d,",
   "percent_change_90d, market_cap, list_src_id, for_date, rank)",
   "VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"]

insertListings :: Connection -> IxValue MetaData -> IO ()
insertListings conn md =
   let lists = toListings md in
   putStrLn (unwords ["Inserting", show (length lists), "listings for",
                      show (date (val md))])             >>
   executeMany conn listingInsertQuery lists             >>
   putStrLn "... done."
