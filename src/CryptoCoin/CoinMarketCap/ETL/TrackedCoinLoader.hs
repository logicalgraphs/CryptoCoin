{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module CryptoCoin.CoinMarketCap.ETL.TrackedCoinLoader where

-- converts a CSV (name,symbol) to a coin to track
-- (because we can buy these coins from binance or coinbase)

-- This is more of an occasional thing, I think.

import Control.Arrow ((&&&))

import qualified Data.ByteString.Char8 as B

import Data.Char (toUpper)

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Maybe (mapMaybe)

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.Types

import System.Environment (getEnv)

import Control.Scan.CSV (rend)

import CryptoCoin.CoinMarketCap.Utils (filesAtDir)

import Data.CryptoCurrency.Types (Idx)

import Store.SQL.Connection (withConnection, Database(ECOIN))
import Store.SQL.Util.LookupTable (lookupTable)
import Store.SQL.Util.Pivots

-- okay, we have a very special lookup table: this lookup table has two-valued
-- lookups: name,symbol that yields the cmc_id. This is from the coin-table

coinLookupQuery :: Query
coinLookupQuery = "SELECT name,symbol,cmc_id FROM coin"

data CoinName = CN String String Idx

instance FromRow CoinName where
   fromRow = CN <$> field <*> field <*> field

type NameSym = (String, String)

toPair :: CoinName -> (NameSym, Integer)
toPair (CN n s i) = ((n, s), i)

type CoinLookup = Map NameSym Integer

coinLookup :: Connection -> IO CoinLookup
coinLookup conn =
   Map.fromList . map toPair <$> query_ conn coinLookupQuery

-- So, from a file, we need to parse the (name, symbol) CSV then upload
-- it based on its type (BINANCE or COINBASE, at present)

insertTrackedCoinsQuery :: Query
insertTrackedCoinsQuery = Query . B.pack $
   unwords ["INSERT INTO j_tracked_coin_tracked_type",
            "(tracked_coin_id, tracked_type_id) VALUES (?, ?)"]

uploadCoinCSV :: FilePath -> Connection -> String -> IO ()
uploadCoinCSV file conn typ =
   lookupTable conn "tracked_type_lk" >>= \types ->
   coinLookup conn                    >>= \coins ->
   coinIds coins <$> readFile file    >>= \idxs  ->
   executeMany conn insertTrackedCoinsQuery
               (map (uncurry Pvt . (,types Map.! typ)) idxs)  >>
   putStrLn (unwords ["Insert",show (length idxs),"tracked coins."])
      where coinIds coins =
               mapMaybe (flip Map.lookup coins . toNameSym . rend ',') . lines
            toNameSym = head &&& last

ucc :: FilePath -> Connection -> IO ()
ucc dir conn = filesAtDir [".csv"] dir >>= mapM_ uploader 
   where uploader file = uploadCoinCSV (dir ++ ('/':file)) conn (typeFrom file)
         typeFrom = map toUpper . fst . break (== '.')

go :: IO ()
go =
   getEnv "COIN_MARKET_CAP_DIR"                   >>= \cmcd ->
   let dir = cmcd ++ "/ETL/trackedCoins" in
   withConnection ECOIN (ucc dir)

{--
>>> cmcDir <- getEnv "COIN_MARKET_CAP_DIR"
>>> go (cmcDir ++ "/ETL/coins_traded_on_binance.csv") "BINANCE"
Insert 32 tracked coins.

>>> go (cmcDir ++ "/ETL/coins_traded_on_coinbase.csv") "COINBASE"
Insert 40 tracked coins.
--}
