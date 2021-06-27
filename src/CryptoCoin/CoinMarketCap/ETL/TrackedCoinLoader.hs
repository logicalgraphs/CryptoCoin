{-# LANGUAGE OverloadedStrings #-}

module CryptoCoin.CoinMarketCap.ETL.TrackedCoinLoader where

{--
Okay, (re)rewrite:

* We need to load what we have in tracked coins data table.
* We need to load the infos from csv files
* diff
* update.
* integrate into Go.go.

That's what we need to do.
--}

{--
import Data.Map (Map)

import Data.Maybe (mapMaybe)

import Data.Set (Set)
import qualified Data.Set as Set

import Database.PostgreSQL.Simple.FromRow
--}

import Control.Arrow ((&&&))

import qualified Data.ByteString.Char8 as B
import Data.Char (toUpper)
import qualified Data.Map as Map

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types (Query(Query))

import System.Environment (getEnv)

import Control.Logic.Frege ((<<-))
import Control.Scan.CSV (csv)

import Data.CryptoCurrency.Types (Idx, Symbol)
import Data.CryptoCurrency.Utils (filesAtDir, report)
import Data.LookupTable (LookupTable)

import Store.SQL.Connection (withConnection, Database(ECOIN))
import Store.SQL.Util.LookupTable (lookupTableFrom)
import Store.SQL.Util.Pivots (Pivot(Pvt))

coinLookupQuery :: Idx -> Query
coinLookupQuery exId = Query . B.pack $ unlines [
   "SELECT c.cmc_id,c.symbol FROM j_tracked_coin_tracked_type j",
   "INNER JOIN coin c ON c.cmc_id=j.tracked_coin_id",
   "WHERE j.tracked_type_id=" ++ show exId]

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

{-- Okay, now load in a CSV file

the file format is:

tracked_type,cmc_id,symbol,name
TERRA,7857,MIR,Mirror Protocol
TERRA,7129,UST,TerraUSD
--}

loadTrackedCoins :: FilePath -> IO LookupTable
loadTrackedCoins file =
   Map.fromList . map (toTup . csv) . tail . lines <$> readFile file
      where toTup (_portfolio:cmcId:sym:_fullNameRest) = (sym, read cmcId)

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

addTrackedCoins :: Connection -> Idx -> [(Symbol, Idx)] -> IO ()
addTrackedCoins _ _ [] = putStrLn "Tracking no new coins."
addTrackedCoins conn exchId adds =
   let ids = zipWith Pvt (map snd adds) (repeat exchId)
   in  report 2 ("Adding new tracked coins: " ++ show (map fst adds)) $
              executeMany conn insertTrackedCoinsQuery ids

-- the coins that are the difference the other way are the coins removed from
-- being tracked

deleteTrackedCoinsQuery :: Query
deleteTrackedCoinsQuery = Query . B.pack $ unlines [
   "DELETE FROM j_tracked_coin_tracked_type",
   "WHERE tracked_coin_id IN ? AND tracked_type_id=?"]

deleteTrackedCoins :: Connection -> Idx -> [(Symbol, Idx)] -> IO ()
deleteTrackedCoins _ _ [] = putStrLn "Not untracking any coins."
deleteTrackedCoins conn exchId deletes =
   report 2 ("Removing coins from being tracked: " ++ show (map fst deletes))
          $ execute conn deleteTrackedCoinsQuery (In $ map snd deletes, exchId)

-- so, now we have all the pieces. Let's assemble them.

diff :: LookupTable -> LookupTable -> [(Symbol, Idx)]
diff = Map.toList <<- Map.difference

uploadCoinCSV :: FilePath -> FilePath -> Connection -> Idx -> IO ()
uploadCoinCSV dir file conn exId =
   let fileName = dir ++ ('/':file) in
   loadTrackedCoins fileName                            >>= \fileCoins ->
   coinLookup conn exId                                 >>= \dbCoins   ->
   report 0 ("For " ++ file ++ ":")
          (addTrackedCoins conn exId (diff fileCoins dbCoins) >>
           deleteTrackedCoins conn exId (diff dbCoins fileCoins))

trackedCoinsQuery :: Query
trackedCoinsQuery = "SELECT tracked_type_id, tracked_type FROM tracked_type_lk"

uploadTrackedCoinsFromCSVs :: Connection -> IO ()
uploadTrackedCoinsFromCSVs conn =
   getEnv "CRYPTOCOIN_DIR"                >>= \cryptDir ->
   lookupTableFrom conn trackedCoinsQuery >>= \types ->
   let dir = cryptDir ++ "/data-files/tracked-coins"
       typeFrom f = types Map.! map toUpper (fst (break (== '.') f))
       uploader file = uploadCoinCSV dir file conn (typeFrom file)
   in  filesAtDir [".csv"] dir >>= mapM_ uploader

go :: IO ()
go = withConnection ECOIN uploadTrackedCoinsFromCSVs
