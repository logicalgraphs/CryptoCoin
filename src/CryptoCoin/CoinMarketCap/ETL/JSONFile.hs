{-# LANGUAGE OverloadedStrings #-}

module CryptoCoin.CoinMarketCap.ETL.JSONFile where

-- types shared across the ETL codebase.

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow

import Data.Aeson

import qualified Data.ByteString.Lazy.Char8 as BL

import qualified Data.Map as Map

import Data.Maybe (mapMaybe)

import Data.LookupTable

import CryptoCoin.CoinMarketCap.Types

import Store.SQL.Connection
import Store.SQL.Util.Indexed
import Store.SQL.Util.LookupTable

extractJSONQuery :: Query
extractJSONQuery =
   "SELECT source_id, file FROM SOURCE WHERE processed=? AND source_type_id=?"

data JSONFile = JSONFile { fileId :: Integer, file :: String }
   deriving (Eq, Show)

instance FromRow JSONFile where
   fromRow = JSONFile <$> field <*> field

extractJSON :: FromJSON a => String -> Connection -> LookupTable
                          -> IO [IxValue a]
extractJSON lk conn srcs =
   query conn extractJSONQuery (False, srcs Map.! lk)             >>=
   return . mapMaybe (\(JSONFile i f) -> IxV i <$> decode (BL.pack f))

extractRanks :: Connection -> LookupTable -> IO [IxValue MetaData]
extractRanks = extractJSON "RANKING"

extractListings :: Connection -> LookupTable -> IO [IxValue MetaData]
extractListings = extractJSON "LISTING"

{--
>>> withConnection ECOIN (\conn -> lookupTable conn "source_type_lk" >>=
                                   extractListings conn              >>=
                                   mapM_ (\(IxV i (MetaData s _)) -> print (i,s)))
(41,Status 2021-03-20 0 Nothing 752 23 Nothing)
(44,Status 2021-03-21 0 Nothing 271 23 Nothing)
(45,Status 2021-03-22 0 Nothing 197 23 Nothing)

... returns only the non-processed listings-files.

Okay, we're extracting and translating the JSON. Now we have to load these
data into the tables... somehow.

Also, note the dates. We have to correct them to local dates before we upload
data twice for one day.

TODO!
--}
