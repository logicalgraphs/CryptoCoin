{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE ViewPatterns      #-}

module CryptoCoin.CoinMarketCap.ETL.CandlestickLoader where

{-- 
We grab the candlesticks for the tracked coins.

This means we need to

1. grab the tracked coins (symbols and cmc_ids)
2. grab the max-date of the candlesticks for each symbol
2. a. if the max date is yesterday, we do nothing (remove the coin from the
      list), because we good.
2. b. (or not 2. b.) if there is no max(for_date), we set it to tday -30
3. for each of the (remaining) coins, we fetch the candlesticks from the
   utcTimeToPOSIXSeconds max-date to yesterday
4. we upload each of those 'files' (marked with the cmc_id) to the source table,
   marked as unprocessed.
--}

import Control.Arrow ((&&&))
import Control.Monad.IO.Class

import qualified Data.ByteString.Char8 as B

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

import Network.HTTP.Req

import Control.Logic.Frege ((-|))

import Control.Scan.CSV (readMaybe, csv)

import CryptoCoin.CoinMarketCap.Data.TrackedCoin (trackedCoins)

import Data.CryptoCurrency.Types (Idx, OCHLV, OCHLVData(OCHLVData), IxRow(IxRow))

import Data.Time.TimeSeries (today)

import Store.SQL.Connection (withConnection, Database(ECOIN))

-- let's get the tracked coins as a LookupTable

import Store.SQL.Util.Indexed hiding (idx)
import Store.SQL.Util.LookupTable

import Data.LookupTable (LookupTable, lookdown)

-- for each of those coins, we need that max(for_date):

type DayTable = Map Integer (String, Day)

fetchMaxDaysQuery :: Query
fetchMaxDaysQuery = Query . B.pack $ unwords [
   "SELECT cmc_id, max(for_date)",
   "FROM candlesticks",
   "WHERE cmc_id IN ?",
   "GROUP BY cmc_id"]

data DayRow = DR { idx :: Integer, day :: Day }

instance FromRow DayRow where
   fromRow = DR <$> field <*> field

maxOr30 :: Connection -> Day -> LookupTable -> IO DayTable
maxOr30 conn (addDays (-1) -> yester) trackeds =
   let thirty sym = (sym, addDays (-30) yester)
       sdekcart   = lookdown trackeds
       symsIdxs = Map.toList trackeds
       idxs   = map snd symsIdxs
       thenMerge dmap = foldr inserter dmap symsIdxs
       inserter (s, i) m = maybe (Map.insert i (thirty s) m) (const m)
                                 (Map.lookup i m)
       tup = sequence . (idx &&& pooh)
       pooh dr = (, day dr) <$> Map.lookup (idx dr) sdekcart
   in  Map.filter ((< yester) . snd)
     . thenMerge
     . Map.fromList
     . mapMaybe tup
     <$> query conn fetchMaxDaysQuery (Only (In idxs))

{--
>>> withConnection ECOIN (\conn -> trackedCoins conn >>= \tc -> 
       today >>= \tday -> maxOr30 conn (addDays (-1) tday) tc >>=
       mapM_ print . Map.toList)
(1,2021-02-27)
(2,2021-02-27)
(74,2021-02-27)
(131,2021-02-27)
(1027,2021-02-27)
(1104,2021-02-27)
...
(6758,2021-02-27)
(6892,2021-02-27)
(7083,2021-02-27)
(7278,2021-02-27)

The url is of this form:

https://query1.finance.yahoo.com/v7/finance/download/BTC-USD
   ?period1=1616711765&period2=1616798165&interval=1d&events=history
   &includeAdjustedClose=true

The result is of the form:

Date,Open,High,Low,Close,Adj Close,Volume
2021-03-25,52726.746094,53392.386719,50856.570313,51704.160156,51704.160156,67999812841
2021-03-26,51683.011719,55137.312500,51579.855469,55137.312500,55137.312500,56652197978
--}

fromCSV  :: Idx -> [String] -> Maybe OCHLV
fromCSV i = fc' i . readMaybe . head <*> map readMaybe . tail

fc' :: Idx -> Maybe Day -> [Maybe Double] -> Maybe OCHLV
fc' i d [o,c,h,l,a,v] =
   IxRow i <$> d
         <*> (OCHLVData <$> o <*> c <*> h <*> l <*> a <*> v)

fetchOCHLV :: Day -> (Idx, (String, Day)) -> IO (Maybe String)
fetchOCHLV (addDays (-1) -> yday) (idx, (sym, addDays 1 -> fromDay)) =
  getCurrentTime >>= \shell ->
  let doNothing _ _ _ = Nothing
      noexceptConfig = defaultHttpConfig { httpConfigCheckResponse = doNothing }
  in  runReq noexceptConfig $ do
  let start, finish :: Integer
      start = floor (utcTimeToPOSIXSeconds (shell { utctDay = fromDay }))
      finish = floor (utcTimeToPOSIXSeconds (shell { utctDay = yday }))
      symUSD = T.pack (concat [sym, "-USD"])
      hist = T.pack "history"
      int = T.pack "1d"
  bs <- req GET (https "query1.finance.yahoo.com"
                       /: "v7" /: "finance" /: "download" /: symUSD)
            NoReqBody bsResponse
          $ "period1" =: start <> "period2" =: finish <> "interval" =: int
            <> "events" =: hist <> "includeAdjustedClose" =: True
  return (responseStatusCode bs == 200 -| Just (B.unpack (responseBody bs)))

{--
>>> today >>= \tday -> fetchOCHLV tday (1, ("BTC", addDays (-5) tday))
Just "Date,Open,High,Low,Close,Adj Close,Volume\n" ++
"2021-03-27,55137.566406,56568.214844,54242.910156,55973.511719,55973.511719,47266542233\n" ++
"2021-03-28,55974.941406,56610.312500,55071.113281,55950.746094,55950.746094,47686580918\n" ++
"2021-03-29,55947.898438,58342.097656,55139.339844,57750.199219,57750.199219,57625587027\n" ++
"2021-03-30,null,null,null,null,null,null"

>>> let (Just resp) = it
>>> mapMaybe (fromCSV 1 . csv) (lines resp)
[OCHLV {coinId = 1, forDay = 2021-03-27, open = 55137.566406, 
        close = 56568.214844, high = 54242.910156, low = 55973.511719, 
        adj = 55973.511719, volume = 4.7266542233e10},
 OCHLV {coinId = 1, forDay = 2021-03-28, open = 55974.941406, 
        close = 56610.3125, high = 55071.113281, low = 55950.746094, 
        adj = 55950.746094, volume = 4.7686580918e10},
 OCHLV {coinId = 1, forDay = 2021-03-29, open = 55947.898438, 
        close = 58342.097656, high = 55139.339844, low = 57750.199219, 
        adj = 57750.199219, volume = 5.7625587027e10}]
--}

storeCandlestickCSVFileQuery :: Query
storeCandlestickCSVFileQuery = Query . B.pack $ unwords [
   "INSERT INTO source (file_name, for_day, file, source_type_id)",
   "VALUES (?, ?, ?, ?) returning source_id"]

-- we also need to store the source-file after we read it

cndlstks :: LookupTable -> Integer
cndlstks srcLk = srcLk Map.! "CANDLESTICKS"

storeCandlestickCSVFile :: Connection -> LookupTable -> String -> Day -> String
                        -> IO Index
storeCandlestickCSVFile conn srcLk sym tday file =
   head <$> query conn storeCandlestickCSVFileQuery
                  (filename, tday, file, cndlstks srcLk)
      where filename = concat [sym, "-candlesticks-", show tday, ".csv"]

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

mkIxOwC :: Index -> Integer -> Idx -> String
        -> Maybe (IxValue OCHLVwithCurrency)
mkIxOwC (Idx srcId) currId cmcId row =
   IxV srcId . flip OwC currId <$> (fromCSV cmcId . csv) row

storeCandlesticks :: Connection -> LookupTable -> LookupTable
                  -> Day -> (Idx, (String, Day)) -> IO ()
storeCandlesticks conn currencyLk srcLk tday row@(cmcId, (sym, _)) =
   fetchOCHLV tday row >>=
   maybe (putStrLn (unwords ["Skipping", sym]))
         (sccf conn srcLk currencyLk sym tday cmcId)

sccf :: Connection -> LookupTable -> LookupTable -> String -> Day -> Idx
     -> String -> IO ()
sccf conn srcLk currencyLk sym tday cmcId file =
   storeCandlestickCSVFile conn srcLk sym tday file >>= \srcId ->
   let makr = mkIxOwC srcId (currencyLk Map.! "USD") cmcId
       rows = mapMaybe makr (lines file)
       msg = unwords ["Storing",show (length rows),"candlesticks for",sym,"..."]
   in  putStrLn msg                                 >>
       executeMany conn storeCandlesticksQuery rows >>
       putStrLn "...done."

storeAllCandlesticks :: Connection -> LookupTable -> LookupTable 
                     -> LookupTable -> IO ()
storeAllCandlesticks conn srcLk currLk trackedCoins =
   today                                                         >>= \tday ->
   maxOr30 conn tday trackedCoins                                >>=
   mapM_ (storeCandlesticks conn currLk srcLk tday) . Map.toList >>
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
   lookupTable conn "source_type_lk"                             >>= \srcLk ->
   lookupTable conn "currency_lk"                                >>= \currLk ->
   trackedCoins conn                                             >>=
   storeAllCandlesticks conn srcLk currLk)

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
