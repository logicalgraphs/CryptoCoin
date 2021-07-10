{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE ViewPatterns      #-}

module CryptoCoin.CoinMarketCap.ETL.Coins.Transformer where

{--
Takes the listing files, processes them into ECoin values, and saves those
values to the data-store.
--}

import Control.Monad (void, foldM)

import qualified Data.ByteString.Char8 as B
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Time (Day)

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.Types

import CryptoCoin.CoinMarketCap.ETL.JSONFile (extractListings)
import CryptoCoin.CoinMarketCap.ETL.ListingLoader (insertListings)
import CryptoCoin.CoinMarketCap.ETL.TagLoader (processTags)
import CryptoCoin.CoinMarketCap.Types

import Data.CryptoCurrency.Types (date, idx, Idx)
import Data.CryptoCurrency.Types.Coin (allCoinIds)
import Data.CryptoCurrency.Utils (report)

import Data.LookupTable (LookupTable)

import Store.SQL.Connection (withConnection, Database(ECOIN))
import Store.SQL.Util.Indexed (IxValue(IxV))
import Store.SQL.Util.LookupTable (lookupTable)

{-- 
The coin table is a lookup table ... with multicolumns for the value against
the index. The only lookupTable construct I have (so far) is a string against
an index, so that's not working, but I need the same functionality for coin
... except I know the index, a priori, because it's assigned from CoinMarketCap.

So. Here we go. From scratch.

Load the coins from the database into a lookup table

No. Because we have the indices already, we just need to do a set-diff
with the indices in the database vs the indices here. The indices here
are the new coins, which we archive.
--}

-- now that we've got the new coins, we can insert them into our coin-table
-- (and token-table) (and rank-table)

-- to insert into the rank-table, we need a source-id and date.

-- Let's just insert into the coin and token tables? But then, how do we
-- do regression analysis? How do we find what the new coins are? From the
-- dailies? I guess that can work...

instance ToRow CoinInfo where
   toRow (CoinInfo i name symbol slug _rank f) =
      [toField i, toField name, toField symbol,
       toField slug, toField f]

insertCoinInfoQuery :: Query
insertCoinInfoQuery = Query . B.pack $ unwords 
   ["INSERT INTO coin (cmc_id, name, symbol, slug, date_added)",
    "VALUES (?, ?, ?, ?, ?)"]

instance ToRow Token where
   toRow (Token coininf parentId tok) =
      [toField (i coininf), toField parentId, toField tok]
         where i (CoinInfo i _ _ _ _ _) = i

insertTokenQuery :: Query
insertTokenQuery = 
   "INSERT INTO token (token_id, parent_id, token_address) VALUES (?,?,?)"

insertNewCoinsQuery :: Query
insertNewCoinsQuery = "INSERT INTO new_coin (cmc_id, for_date) VALUES (?, ?)"

insertNewCoinsDate :: Connection -> Day -> [ECoin] -> IO ()
insertNewCoinsDate conn date =
   void . executeMany conn insertNewCoinsQuery . map (toDate date)

toDate :: Day -> ECoin -> Date
toDate d (idx -> i) = Dt i d

data Date = Dt Integer Day

instance ToRow Date where
   toRow (Dt i d) = [toField i, toField d]

insertCoinInfos :: Connection -> [ECoin] -> IO ()
insertCoinInfos conn = void . executeMany conn insertCoinInfoQuery . map info

insertTokens :: Connection -> [ECoin] -> IO ()
insertTokens conn = void . executeMany conn insertTokenQuery . mapMaybe unToken

unToken :: ECoin -> Maybe Token
unToken (T tok) = Just tok
unToken _       = Nothing

-- so, this is how we do it.

-- We insert all the coin information first, then we insert the tokens
-- (which are references to the token (coininfo) and their parent coin(info))

insertAllCoins :: Connection -> Day -> [ECoin] -> IO ()
insertAllCoins conn date allCoins =
   report 0 ("Inserting " ++ show (length allCoins) ++ " coin infos.")
            (insertCoinInfos conn allCoins >> insertTokens conn allCoins) >>
   report 0 "Inserting all new coins and tokens into new_coin table"
            (insertNewCoinsDate conn date allCoins)

deleteListingsStmt :: Day -> Query
deleteListingsStmt date = Query . B.pack $ concat [
   "DELETE FROM coin_market_cap_daily_listing WHERE for_date='", show date, "'"]

deleteListings :: Connection -> Day -> IO ()
deleteListings conn = void . execute_ conn . deleteListingsStmt

processOneListingFile :: Connection -> Set Idx -> IxValue MetaData
                      -> IO (Set Idx)
processOneListingFile conn coinz i@(IxV _ md) =
   let nc = newCoins md coinz in
   putStrLn ("\n\nFor listing file " ++ show (date md) ++ ":") >>
   insertAllCoins conn (date md) nc                            >>
   deleteListings conn (date md)                               >>
   insertListings conn i                                       >>
   processTags conn i                                          >>
   return (Set.union coinz (Set.fromList (map idx nc)))

newCoins :: MetaData -> Set Idx -> [ECoin]
newCoins (MetaData _ listings) =
   map coin . Map.elems . foldr Map.delete listings . Set.toList

setProcessed :: Connection -> LookupTable -> IO ()
setProcessed conn srcs =
   execute conn "UPDATE source SET processed=? WHERE source_type_id=?"
           (True, srcs Map.! "LISTING") >>
   putStrLn "Set all listings files as processed."

processFiles :: Connection -> IO ()
processFiles conn =
   report 0 "Processing coin listings."
          (lookupTable conn "source_type_lk"      >>= \srcs ->
           allCoinIds conn                        >>= \acz ->
           extractListings conn srcs              >>=
           foldM (processOneListingFile conn) acz >>
           setProcessed conn srcs)

go :: IO ()
go = withConnection ECOIN processFiles
