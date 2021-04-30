{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE ViewPatterns      #-}

module CryptoCoin.CoinMarketCap.ETL.NewCoinTransformer where

{--
Takes the listing files, processes them into ECoin values, and saves those
values to the data-store.
--}

import Control.Monad (forM_, void)

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.Types

import qualified Data.ByteString.Char8 as B

import qualified Data.Map as Map

import Data.Time (Day)

import Data.CryptoCurrency.Types hiding (Date)      -- Idx

import CryptoCoin.CoinMarketCap.ETL.Coins.NewCoins (newCoins)
import CryptoCoin.CoinMarketCap.ETL.JSONFile (extractListings)
import CryptoCoin.CoinMarketCap.ETL.ListingLoader (insertListings)
import CryptoCoin.CoinMarketCap.ETL.TagLoader (processTags)
import CryptoCoin.CoinMarketCap.Types

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

insertCoin :: Connection -> ECoin -> IO ()
insertCoin conn ecoin =
   insertCoinInfo conn (info ecoin) >> thenInsertCoin conn ecoin

insertCoinInfo :: Connection -> CoinInfo -> IO ()
insertCoinInfo conn = void . execute conn insertCoinInfoQuery

thenInsertCoin :: Connection -> ECoin -> IO ()
thenInsertCoin _ (C _) = return ()
thenInsertCoin conn (T tok) = execute conn insertTokenQuery tok >> return ()

-- so, this is how we do it.

-- We insert all the coins first, then we insert the tokens

insertAllCoins :: Connection -> Day -> NewCoins -> IO ()
insertAllCoins conn date nc@(coins, tokens) =
   putStrLn ("Inserting " ++ show (length coins) ++ " coins.")                >>
   forM_ coins (insertCoin conn)                                              >>
   putStrLn "...done."                                                        >>
   putStrLn ("Inserting " ++ show (length tokens) ++ " tokens.")              >>
   forM_ tokens (insertCoin conn)                                             >>
   putStrLn "Inserting all new coins and tokens into new_coin table"          >>
   executeMany conn insertNewCoinsQuery (map (toDate date) (coins ++ tokens)) >>
   putStrLn "...done."

toDate :: Indexed i => Day -> i -> Date
toDate d (idx -> i) = Dt i d

data Date = Dt Integer Day

instance ToRow Date where
   toRow (Dt i d) = [toField i, toField d]

processOneListingFile :: Connection -> IxValue MetaData -> IO ()
processOneListingFile conn i@(IxV _ md) =
   putStrLn ("\n\nFor listing file " ++ show (date md) ++ ":") >>
   newCoins conn md                                            >>=
   insertAllCoins conn (date md)                               >>
   insertListings conn i                                       >>
   processTags conn i

setProcessed :: Connection -> LookupTable -> IO ()
setProcessed conn srcs =
   execute conn "UPDATE source SET processed=? WHERE source_type_id=?"
           (True, srcs Map.! "LISTING") >>
   putStrLn "Set all listings files as processed."

-- Process all of them:

processFiles :: Connection -> LookupTable -> IO ()
processFiles conn srcs =
   putStrLn "Processing coin listings." >>   
   extractListings conn srcs            >>=
   mapM_ (processOneListingFile conn)   >>
   setProcessed conn srcs

go :: IO ()
go =
   withConnection ECOIN (\conn ->
      lookupTable conn "source_type_lk" >>= processFiles conn)
