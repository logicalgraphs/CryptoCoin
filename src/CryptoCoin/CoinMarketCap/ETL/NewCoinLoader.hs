{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module CryptoCoin.CoinMarketCap.ETL.NewCoinLoader where

import Control.Arrow (second, (&&&))
import Control.Monad (forM_)

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.Types

import qualified Data.ByteString.Char8 as B

import Data.Int (Int64)

import Data.List (partition)

import Data.Map (Map)
import qualified Data.Map as Map

import CryptoCoin.CoinMarketCap.Types
import CryptoCoin.CoinMarketCap.ETL.JSONFile
import CryptoCoin.CoinMarketCap.ETL.ListingLoader (insertListings)

import Data.CryptoCurrency.Types hiding (idx)      -- Idx

import Data.LookupTable

import Store.SQL.Connection
import Store.SQL.Util.Indexed
import Store.SQL.Util.LookupTable

{-- 
Remember we need to set processed to true when we're all done here!

The coin table is a lookup table ... with multicolumns for the value against
the index. The only lookupTable construct I have (so far) is a string against
an index, so that's not working, but I need the same functionality for coin
... except I know the index, a priori, because it's assigned from
CoinMarketCap.

So. Here we go. From scratch.

Load the coins from the database into a lookup table

No. Because we have the indices already, we just need to do a set-diff
with the indices in the database vs the indices here. The indices here
are the new coins, which we archive.
--}

newCoins :: Connection -> MetaData -> IO (Map Idx Listing)
newCoins conn (MetaData _ m) =
   foldr Map.delete m . map (fromIntegral . idx) <$> coins conn

-- to do that, we need to extract the indices from the database, ... with
-- (any other) value

coins :: Connection -> IO [Index]
coins conn = query_ conn "SELECT cmc_id FROM coin"

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

insertCoin :: Connection -> ECoin -> IO ()
insertCoin conn ecoin =
   insertCoinInfo conn (info ecoin) >> thenInsertCoin conn ecoin

insertCoinInfo :: Connection -> CoinInfo -> IO Int64
insertCoinInfo conn = execute conn insertCoinInfoQuery

thenInsertCoin :: Connection -> ECoin -> IO ()
thenInsertCoin _ (C _) = return ()
thenInsertCoin conn (T tok) = execute conn insertTokenQuery tok >> return ()

-- so, this is how we do it.

-- We insert all the coins first, then we insert the tokens

insertAllCoins :: Connection -> [ECoin] -> IO NewCoins
insertAllCoins conn ecoins =
   let (tokens, coins) = partition isToken ecoins in
   putStrLn ("Inserting " ++ show (length coins) ++ " coins.")   >>
   forM_ coins (insertCoin conn)                                 >>
   putStrLn "...done."                                           >>
   putStrLn ("Inserting " ++ show (length tokens) ++ " tokens.") >>
   forM_ tokens (insertCoin conn)                                >>
   putStrLn "...done."                                           >>
   return (coins, tokens)

processOneListingFile :: Connection -> IxValue MetaData -> IO NewCoins
processOneListingFile conn i@(IxV _ md) =
   putStrLn ("\n\nFor listing file " ++ show (date md) ++ ":") >>
   newCoins conn md                                            >>=
   insertAllCoins conn . map coin . Map.elems                  >>= \ans ->
   insertListings conn i                                       >>
   return ans

setProcessed :: Connection -> LookupTable -> IO ()
setProcessed conn srcs =
   execute conn "UPDATE source SET processed=? WHERE source_type_id=?"
           (True, srcs Map.! "LISTING") >>
   putStrLn "Set all listings files as processed."

-- Process all of them:

go :: IO ()
go =
   withConnection ECOIN (\conn ->
      lookupTable conn "source_type_lk" >>= processFiles conn)

processFiles :: Connection -> LookupTable -> IO [NewCoinsCtx]
processFiles conn srcs =
      extractListings conn srcs                                 >>=
      traverse (sequence . (id &&& processOneListingFile conn)) >>= \newsies ->
      setProcessed conn srcs                                    >>
      return newsies
