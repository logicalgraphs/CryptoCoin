{-# LANGUAGE OverloadedStrings #-}

module CryptoCoin.CoinMarketCap.ETL.TagLoader where

{--
We need to get our tags-context, we need to extract all tags-coins, we need
to store new tags (with new ids), we need store tags-coins in the pivot table.

Easy enough.
--}

import Control.Arrow ((&&&))

import Data.Map (Map)
import Data.Set (Set)

import Database.PostgreSQL.Simple

import Data.CryptoCurrency.Types (Idx)
import Data.LookupTable

import Store.SQL.Util.Indexed
import Store.SQL.Util.LookupTable

import CryptoCoin.CoinMarketCap.Types

processTags :: Connection -> IxValue MetaData -> IO ()
processTags conn (IxV _ (MetaData _ listings)) =
   let workingTags = matchTags listings in
   fetchTags conn >>= \tagCtx ->
   undefined

-- step 1: get the tags from the database, including the next index

fetchTags :: Connection -> IO (LookupTable, Integer)
fetchTags conn = (id &&& nextIndex) <$> lookupTable conn "tag"

-- step 2: get the tags associated to each listing

matchTags :: Map Idx Listing -> Map Tag (Set Idx)
matchTags listings = undefined
