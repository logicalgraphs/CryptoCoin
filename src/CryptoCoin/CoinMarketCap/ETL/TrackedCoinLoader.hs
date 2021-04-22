{-# LANGUAGE OverloadedStrings #-}

module CryptoCoin.CoinMarketCap.ETL.TrackedCoinLoader where

{--
Okay, rewrite:

* We need to load what we have in tracked coins data table.
* We need to load the infos from csv files
* diff
* update.
* integrate into Go.go.

That's what we need to do.
--}

import Control.Arrow ((&&&))

import qualified Data.ByteString.Char8 as B

import Data.Char (toUpper)

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Maybe (mapMaybe)

import Data.Set (Set)
import qualified Data.Set as Set

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.Types (Query(Query))

import System.Environment (getEnv)

import Control.Logic.Frege ((<<-))
import Control.Scan.CSV (csv)

import CryptoCoin.CoinMarketCap.Utils (filesAtDir, report)

import Data.CryptoCurrency.Types (Idx)
import Data.LookupTable (LookupTable)

import Store.SQL.Connection (withConnection, Database(ECOIN))
import Store.SQL.Util.LookupTable (lookupTable, lookupTableFrom)
import Store.SQL.Util.Pivots (Pivot(Pvt))

-- okay, we have a very special lookup table: this lookup table has two-valued
-- lookups: name,symbol that yields the cmc_id. This is from the coin-table

coinLookupQuery :: Idx -> Query
coinLookupQuery kind = Query . B.pack $ unwords [
   "SELECT c.cmc_id,c.symbol FROM coin c",
   "INNER JOIN j_tracked_coin_tracked_type j ON c.cmc_id=j.tracked_coin_id",
   "WHERE j.tracked_type_id=", show kind]

coinLookup :: Connection -> Idx -> IO LookupTable
coinLookup conn = lookupTableFrom conn . coinLookupQuery

{--
>>> :set -XOverloadedStrings 
>>> withConnection ECOIN (\conn ->
            lookupTableFrom conn trackedCoinsQuery         >>=
                 coinLookup conn . flip (Map.!) "COINBASE" >>=
                 mapM_ print . Map.toList)
("AAVE",7278)
("ADA",2010)
("ALGO",4030)
("ANKR",3783)
("ATOM",3794)
("BAL",5728)
("BAND",4679)
("BAT",1697)
("BCH",1831)
("BTC",1)
...

>>> withConnection ECOIN (\conn ->
            lookupTableFrom conn trackedCoinsQuery        >>=
                 coinLookup conn . flip (Map.!) "BINANCE" >>=
                 mapM_ print . Map.toList)
("ADA",2010)
("ALGO",4030)
("ATOM",3794)
("BAND",4679)
("BAT",1697)
("BCH",1831)
("BTC",1)
...
--}

-- Okay, now load in a CSV file

loadTrackedCoins :: FilePath -> IO (Set String)
loadTrackedCoins file =
   Set.fromList . map (last . csv) . tail . lines <$> readFile file

{--
>>> dir <- (++ "/data-files/tracked-coins") <$> getEnv "CRYPTOCOIN_DIR"
>>> loadTrackedCoins (dir ++ "/binance.csv")
fromList ["ADA","ALGO","ATOM","BAND","BAT","BCH","BNB","BTC",...]

The coins that are the difference one way are the newly tracked coins.
--}

insertTrackedCoinsQuery :: Query
insertTrackedCoinsQuery = Query . B.pack $
   unwords ["INSERT INTO j_tracked_coin_tracked_type",
            "(tracked_coin_id, tracked_type_id) VALUES (?, ?)"]

diff :: Set String -> Set String -> [String]
diff = Set.toList <<- Set.difference

addTrackedCoins :: Connection -> Idx -> LookupTable -> [String] -> IO ()
addTrackedCoins _ _ _ [] = putStrLn "Tracking no new coins."
addTrackedCoins conn exchId allCoinz syms =
   let newIds = mapMaybe (flip Map.lookup allCoinz) syms  -- *
       ids = zipWith Pvt newIds (repeat exchId)
   in  report 2 ("Adding new tracked coins: " ++ show syms) $
              executeMany conn insertTrackedCoinsQuery ids

-- * We're going to pretend, for now, that there is one symbol per coin, smh.

-- the coins that are the difference the other way are the coins removed from
-- being tracked

deleteTrackedCoinsQuery :: Query
deleteTrackedCoinsQuery = Query . B.pack $ unwords [
   "DELETE FROM j_tracked_coin_tracked_type",
   "WHERE tracked_coin_id IN ? AND tracked_type_id=?"]

deleteTrackedCoins :: Connection -> Idx -> LookupTable -> [String] -> IO ()
deleteTrackedCoins _ _ _ [] = putStrLn "Not untracking any coins."
deleteTrackedCoins conn exchId dbktz deletes =
   let ids = mapMaybe (flip Map.lookup dbktz) deletes
   in  report 2 ("Removing coins from being tracked: " ++ show deletes) $
       execute conn deleteTrackedCoinsQuery (In ids, exchId)

-- so, now we have all the pieces. Let's assemble them.

allCoinsQuery :: Query
allCoinsQuery = "SELECT cmc_id,symbol FROM coin"

allCoins :: Connection -> IO LookupTable
allCoins = flip lookupTableFrom allCoinsQuery

{--
>>> withConnection ECOIN (\conn -> allCoins conn >>=
                                   mapM_ print . take 5 . Map.toList)
("$ANRX",8057)
("$BASED",6570)
("$COIN",7796)
("$KING",7569)
("$NOOB",7646)
--}

uploadCoinCSV :: LookupTable -> FilePath -> FilePath -> Connection -> Idx
              -> IO ()
uploadCoinCSV allCoins dir file conn typ =
   let fileName = dir ++ ('/':file) in
   loadTrackedCoins fileName                            >>= \fileCoins ->
   coinLookup conn typ                                  >>= \dbCoins   ->
   let mksDbCoins = Map.keysSet dbCoins in
   putStrLn ("For " ++ file ++ ":")                              >>
   addTrackedCoins conn typ allCoins (diff fileCoins mksDbCoins) >>
   deleteTrackedCoins conn typ dbCoins (diff mksDbCoins fileCoins)

trackedCoinsQuery :: Query
trackedCoinsQuery = "SELECT tracked_type_id, tracked_type FROM tracked_type_lk"

uploadTrackedCoinsFromCSVs :: Connection -> IO ()
uploadTrackedCoinsFromCSVs conn =
   getEnv "CRYPTOCOIN_DIR"                >>= \cryptDir ->
   allCoins conn                          >>= \allCoinz ->
   lookupTableFrom conn trackedCoinsQuery >>= \types ->
   let dir = cryptDir ++ "/data-files/tracked-coins"
       uploader file = uploadCoinCSV allCoinz dir file conn (typeFrom file)
       typeFrom f = types Map.! map toUpper (fst (break (== '.') f))
   in  filesAtDir [".csv"] dir >>= mapM_ uploader

go :: IO ()
go = withConnection ECOIN uploadTrackedCoinsFromCSVs
