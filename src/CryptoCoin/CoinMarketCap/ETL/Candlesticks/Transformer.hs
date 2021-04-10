{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE ViewPatterns      #-}

module CryptoCoin.CoinMarketCap.ETL.Candlesticks.Transformer where

{-- 
Now that we've loaded the CSV for the Candlesticks to our data-store, let's
process these files, ... named as follows:

sym-cmc_id-candlesticks-date.csv

We process the CSV file, extracting the candlestick information, the load those
entities into the database.

import Control.Arrow ((&&&))
import Control.Monad.IO.Class

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Maybe (mapMaybe)

import Data.Monoid

import qualified Data.Text as T

import Data.Time (Day, addDays, utctDay, getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.Types

import CryptoCoin.CoinMarketCap.Data.TrackedCoin (trackedCoins)
import Data.CryptoCurrency.Types (Idx, IxRow(IxRow))
import Data.CryptoCurrency.Types.OCHLV (OCHLVData(OCHLVData), OCHLV)

-- let's get the tracked coins as a LookupTable

import Store.SQL.Util.Indexed
import Store.SQL.Util.LookupTable

import Data.LookupTable (LookupTable, lookdown)
--}

import qualified Data.ByteString.Char8 as B

import qualified Data.Map as Map

import Data.Maybe (mapMaybe)

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.Types (Query(Query))

import Control.Scan.CSV (csv, rend)

import CryptoCoin.CoinMarketCap.ETL.Candlesticks.Util (fromCSV, cndlstks)

import Data.CryptoCurrency.Types (Cymbal, sym, IxRow(IxRow))
import Data.CryptoCurrency.Types.OCHLV (OCHLVData(OCHLVData), OCHLV)

import Data.LookupTable (LookupTable)

import Store.SQL.Connection (withConnection, Database(ECOIN))
import Store.SQL.Util.Indexed (IxValue(IxV), Index(Idx), Indexed, idx)
import Store.SQL.Util.LookupTable (lookupTable)

-- First, we have to load the unprocessed CSV files

fetchCandlestickCSVFilesQuery :: Query
fetchCandlestickCSVFilesQuery =
   "SELECT file_name, file FROM source WHERE processed=? AND source_type_id=?"

data CSVFile' = CSVFile' String String
   deriving Show

instance FromRow CSVFile' where
   fromRow = CSVFile' <$> field <*> field

instance Indexed CSVFile' where
   idx (CSVFile' f _) = read . head . tail $ rend '-' f

instance Cymbal CSVFile' where
   sym (CSVFile' f _) = head $ rend '-' f

data CSVFile = CSVFile String String
   deriving Show

instance Cymbal CSVFile where
   sym (CSVFile s _) = s

toIxCSVFile :: CSVFile' -> IxValue CSVFile
toIxCSVFile c@(CSVFile' _ file) = IxV (idx c) (CSVFile (sym c) file)

fetchCandlestickCSVFiles :: Connection -> LookupTable -> IO [IxValue CSVFile]
fetchCandlestickCSVFiles conn srclks =
   map toIxCSVFile
   <$> query conn fetchCandlestickCSVFilesQuery (False, cndlstks srclks)

-- and now: we process them.

storeCandlesticksQuery :: Query
storeCandlesticksQuery = Query . B.pack $ unwords [
   "INSERT INTO candlesticks (source_id, cmc_id, for_date, open, high, low,",
   "close, adjusted_close, volume, currency_id)",
   "VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"]

instance ToRow r => ToRow (IxRow r) where
   toRow (IxRow i d r) = [toField i, toField d] ++ toRow r

instance ToRow OCHLVData where
   toRow (OCHLVData o c h l ad v) = map toField [o, h, l, c, ad, v]

data OCHLVwithCurrency = OwC OCHLV Integer

instance ToRow OCHLVwithCurrency where
   toRow (OwC o c) = toRow o ++ [toField c]

mkIxOwC :: Index -> Integer -> Integer -> String
        -> Maybe (IxValue OCHLVwithCurrency)
mkIxOwC (Idx srcId) currId cmcId row =
   IxV srcId . flip OwC currId <$> (fromCSV cmcId . csv) row

storeCandlesticks :: Connection -> LookupTable -> LookupTable
                  -> IxValue CSVFile -> IO ()
storeCandlesticks conn currencyLk srcLk (IxV cmcId (CSVFile sym file)) =
   let srcId = Idx (cndlstks srcLk)
       makr = mkIxOwC srcId (currencyLk Map.! "USD") cmcId
       rows = mapMaybe makr (lines file)
       msg = unwords ["Storing",show (length rows),"candlesticks for",sym,"..."]
   in  putStrLn msg                                 >>
       executeMany conn storeCandlesticksQuery rows >>
       putStrLn "...done."

processAllCandlesticks :: Connection -> LookupTable -> LookupTable -> IO ()
processAllCandlesticks conn srcLk currLk =
   fetchCandlestickCSVFiles conn srcLk                   >>= \cndls ->
   let msg = unwords ["Processing", show (length cndls), "candlestick files."]
   in  putStrLn msg                                      >>
       mapM_ (storeCandlesticks conn currLk srcLk) cndls >>
       processedCandlesticks conn srcLk

processedCandlesticksQuery :: Query
processedCandlesticksQuery =
   "UPDATE source SET processed=? WHERE source_type_id=?"

processedCandlesticks :: Connection -> LookupTable -> IO ()
processedCandlesticks conn srcLk =
   execute conn processedCandlesticksQuery (True, cndlstks srcLk) >>
   putStrLn "All candlesticks files set to processed."

go :: IO ()
go = withConnection ECOIN (\conn ->
   let lk = lookupTable conn in
   processAllCandlesticks conn <$> lk "source_type_lk" <*> lk "currency_lk")

{--
>>> go
Storing 30 candlesticks for BTC ...
...done.
Storing 30 candlesticks for LTC ...
...done.
Storing 30 candlesticks for DOGE ...
...done.
Storing 30 candlesticks for DASH ...
...done.
Storing 30 candlesticks for ETH ...
...done.
Storing 30 candlesticks for REP ...
...done.
Storing 30 candlesticks for WAVES ...
...done.
Storing 30 candlesticks for ZEC ...
...done.
Skipping MKR
Storing 30 candlesticks for BAT ...
...done.
Skipping NMR
...
Storing 30 candlesticks for EGLD ...
...done.
Skipping UNI
Skipping AAVE
All candlesticks files set to processed.
--}
