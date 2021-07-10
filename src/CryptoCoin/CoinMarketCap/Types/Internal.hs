{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module CryptoCoin.CoinMarketCap.Types.Internal where

-- translates raw-JSON structures or database rows to our Haskell-y types

import Data.Aeson

import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Foldable (toList)

import Data.Map (Map)

import Data.Time (Day)

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.Types

import System.Environment (getEnv)

import Data.CryptoCurrency.Types   -- for indexed
import CryptoCoin.CoinMarketCap.Types.Quote

type TokenAddress = String

data CoinRef' = CR' Idx TokenAddress
   deriving (Eq, Ord, Show)

instance Indexed CoinRef' where
   idx (CR' i _) = i

instance FromJSON CoinRef' where
   parseJSON = withObject "ref" $ \v ->
      CR' <$> v .: "id" <*> v .: "token_address"

data Listing' =
   Listing' Integer String String String
            (Maybe Integer) String Double Double (Maybe Double) [String]
            (Maybe CoinRef') (Maybe Integer)
            (Map String Quote)
      deriving Show

plat :: Listing' -> Maybe CoinRef'
plat (Listing' _ _ _ _ _ _ _ _ _ _ p _ _) = p

instance FromJSON Listing' where
   parseJSON = withObject "listing" $ \v ->
      Listing' <$> v .: "id" <*> v .: "name" <*> v .: "symbol" <*> v .: "slug"
               <*> v .:? "num_market_pairs" <*> v .: "date_added"
               <*> v .: "circulating_supply" <*> v .: "total_supply"
               <*> v .:? "max_supply" <*> v .: "tags" <*> v .:? "platform"
               <*> v .:? "cmc_rank" <*> v .: "quote"

sample :: String -> IO ByteString
sample thing =
   getEnv "COIN_MARKET_CAP_DIR" >>=
   BL.readFile . (++ "/ETL/sample" ++ thing ++ ".json")

-- and the database-side ---------------------------------------------

data ListingDB =
   ListingDB (Maybe Integer) (Maybe Double) Double Double Integer Quote
      deriving (Eq, Ord, Show)

instance FromRow ListingDB where
   fromRow = ListingDB <$> field <*> field <*> field
                       <*> field <*> field <*> fromRow

fetchListingDBsQuery :: Day -> Query
fetchListingDBsQuery date =
   Query . B.pack $ unlines (fetchListingsBase date "AND cmc_id IN ?")

fetchListingsBase :: Day -> String -> [String]
fetchListingsBase date continuation = [
   "SELECT cmc_id, for_date, num_pairs, max_supply, circulating_supply,",
   "total_supply, rank, quote_price, volume_24h, percent_change_1h,",
   "percent_change_24h, percent_change_7d, percent_change_30d,",
   "percent_change_60d, percent_change_90d, market_cap",
   "FROM coin_market_cap_daily_listing",
   "WHERE for_date='" ++ show date ++ "' " ++ continuation]

type IxListingDB = IxRow ListingDB

fetchListingDBs :: Foldable t => Connection -> Day -> t Idx -> IO [IxListingDB]
fetchListingDBs conn date =
   query conn (fetchListingDBsQuery date) . Only . In . toList

fetchTop10ListingDBs :: Connection -> Day -> IO [IxListingDB]
fetchTop10ListingDBs conn date = query_ conn (fetchTop10Query date)

fetchTop10Query :: Day -> Query
fetchTop10Query date =
   Query . B.pack $ unlines (fetchListingsBase date "ORDER BY rank LIMIT 10")
