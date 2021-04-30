{-# LANGUAGE OverloadedStrings #-}

module CryptoCoin.CoinMarketCap.ETL.TagLoader where

{--
We need to get our tags-context, we need to extract all tags-coins, we need
to store new tags (with new ids), we need store tags-coins in the pivot table.

Easy enough.
--}

import Control.Arrow ((&&&), second)

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Tuple (swap)

import Database.PostgreSQL.Simple

import Control.Map (snarf)

import Data.CryptoCurrency.Types (Idx)
import Data.LookupTable

import Store.SQL.Connection (withConnection, Database(ECOIN))
import Store.SQL.Util.Indexed (IxValue(IxV))
import Store.SQL.Util.LookupTable
import Store.SQL.Util.Pivots

import CryptoCoin.CoinMarketCap.Types
import CryptoCoin.CoinMarketCap.ETL.JSONFile

processTags :: Connection -> IxValue MetaData -> IO ()
processTags conn (IxV _ (MetaData _ listings)) =
   fetchAllTags conn >>= \(lk, nxt) ->
   let workingTags = matchTags listings
       n00bz       = newTags' nxt (newTags workingTags lk) in
   loadNewTags conn n00bz           >>
   deleteOldPivots conn workingTags >>
   andPivotTags conn (tagPivotValues lk n00bz workingTags)

-- step 1: get the tags from the database, including the next index

fetchAllTags :: Connection -> IO (LookupTable, Idx)
fetchAllTags conn = (id &&& nextIndex) <$> lookupTable conn "tag"

-- step 2: get the tags associated to each listing

matchTags :: Map Idx Listing -> Map Tag (Set Idx)
matchTags = snarf return
          . map swap
          . (>>= traverse id)
          . map (second tags)
          . Map.toList

-- step 3: new tags

newTags :: Map Tag (Set Idx) -> LookupTable -> Map Tag (Set Idx)
newTags tday = foldr Map.delete tday . Map.keys

-- step 4: add the new tags to the database and to the lookup-table.

-- step 4a: create the new tags-ids

newTags' :: Idx -> Map Tag (Set Idx) -> LookupTable
newTags' nextIdx = Map.fromList . map swap . zip [nextIdx ..] . Map.keys

-- step 4b: load these N00bz into the database

loadNewTags :: Connection -> LookupTable -> IO ()
loadNewTags conn newTags =
   executeMany conn "INSERT INTO tag (tag_name, tag_id) VALUES (?, ?)"
               (Map.toList newTags) >>
   putStrLn (unwords ["Insert", show (length newTags), "new tags."])

-- step 4c: Now merge the results into a new lookup table (Map.union)
-- and use that to form the pivot results for upload

tagPivotValues :: LookupTable -> LookupTable -> Map Tag (Set Idx) -> [Pivot]
tagPivotValues big news =
   map (uncurry Pvt)
   . (>>= traverse Set.toList)
   . Map.toList
   . Map.mapKeys (\tag -> (Map.union big news) Map.! tag)

-- but, let's first clear the air, removing all prior pivots from the coins
-- ranked today.

deleteOldPivots :: Connection -> Map Tag (Set Idx) -> IO ()
deleteOldPivots conn tags =
   let cmcIds = Set.toList (Set.unions (Map.elems tags)) in
   execute conn "DELETE FROM j_tag_coin WHERE cmc_id IN ?" (Only (In cmcIds)) >>
   putStrLn (unwords ["Deleted tag relations for",
                      show (length cmcIds), "coins."])

-- Now we add the tag-relations for the coin-ids

andPivotTags :: Connection -> [Pivot] -> IO ()
andPivotTags con ps =
   executeMany con "INSERT INTO j_tag_coin (tag_id, cmc_id) VALUES (?, ?)" ps >>
   putStrLn (unwords ["Insert", show (length ps), "tag-relations."])

go :: IO ()
go = withConnection ECOIN (\conn ->
   lookupTable conn "source_type_lk" >>=
   extractListings conn              >>=
   mapM_ (processTags conn))
