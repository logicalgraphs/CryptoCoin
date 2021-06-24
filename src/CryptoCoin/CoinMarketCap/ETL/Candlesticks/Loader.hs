{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE ViewPatterns      #-}

module CryptoCoin.CoinMarketCap.ETL.Candlesticks.Loader where

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

Okay, and then we're going to

5. process the unprocessed files in the database.

But we're going to do 5. in Candlesticks.Transformer.
--}

import Control.Arrow ((&&&), (***))
import Control.Monad (void)

import qualified Data.ByteString.Char8 as B

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Maybe (mapMaybe)

import Data.Monoid ((<>))

import qualified Data.Text as T

import Data.Time (Day, addDays, utctDay, getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)

import Network.HTTP.Req

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.Types (Query(Query))

import Control.Logic.Frege ((-|))

import CryptoCoin.CoinMarketCap.Data.TrackedCoin (trackedCoins)
import CryptoCoin.CoinMarketCap.ETL.Candlesticks.Util (cndlstks)
import CryptoCoin.CoinMarketCap.Utils (geaux)

import Data.CryptoCurrency.Types (Idx, IxRow(IxRow))
import Data.CryptoCurrency.Utils (report)

import Data.LookupTable (LookupTable, lookdown)

import Data.Time.TimeSeries (today)

import Store.SQL.Connection (withConnection, Database(ECOIN))
import Store.SQL.Util.Indexed hiding (idx)
import Store.SQL.Util.LookupTable (lookupTable)

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

maxOr250 :: Connection -> Day -> LookupTable -> IO DayTable
maxOr250 conn (addDays (-1) -> yester) trackeds =
   let twoFifty sym = (sym, addDays (-250) yester)
       sdekcart   = lookdown trackeds
       symsIdxs = Map.toList trackeds
       idxs   = map snd symsIdxs
       thenMerge dmap = foldr inserter dmap symsIdxs
       inserter (s, i) m = maybe (Map.insert i (twoFifty s) m) (const m)
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
       today >>= \tday -> maxOr250 conn (addDays (-1) tday) tc >>=
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

fetchOCHLV :: Day -> (Idx, (String, Day)) -> IO (Maybe String)
fetchOCHLV (addDays (-1) -> yday) (idx, (sym, addDays 1 -> fromDay)) =
  getCurrentTime >>= \shell ->
  let doNothing _ _ _ = Nothing
      noexceptConfig = defaultHttpConfig { httpConfigCheckResponse = doNothing }
  in  runReq noexceptConfig $ do
  let getDate :: Day -> Integer
      getDate date = floor (utcTimeToPOSIXSeconds (shell { utctDay = date }))
      start = getDate fromDay
      finish = getDate yday
      symUSD = T.pack (concat [sym, "-USD"])
      hist = T.pack "history"
      int = T.pack "1d"
  bs <- req GET (https "query1.finance.yahoo.com"
                       /: "v7" /: "finance" /: "download" /: symUSD)
            NoReqBody bsResponse
          $ "period1" =: start <> "period2" =: finish <> "interval" =: int
            <> "events" =: hist <> "includeAdjustedClose" =: True
  return (responseStatusCode bs == 200 -| B.unpack (responseBody bs))

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

-- we also need to store the source-file after we read it

storeCandlestickCSVFileQuery :: Query
storeCandlestickCSVFileQuery = Query . B.pack $ unwords [
   "INSERT INTO source (file_name, for_day, file, source_type_id)",
   "VALUES (?, ?, ?, ?)"]

maybeStoreCandlestickCSVFile :: Connection -> LookupTable -> Day 
                             -> ((Idx, String), Maybe String) -> IO ()
maybeStoreCandlestickCSVFile conn srcLk tday ((idx, sym), Just file) =
   execute conn storeCandlestickCSVFileQuery
                (filename, tday, file, cndlstks srcLk) >>
   putStrLn ("Saving candlesticks for " ++ sym)
      where filename = concat [sym, '-':show idx, "-candlesticks-",
                               show tday, ".csv"]
maybeStoreCandlestickCSVFile _ _ _ ((_, sym), Nothing) =
   putStrLn (unwords ["Skipping", sym])

-- but we don't need no index when we store it if we know the cmc_id. Which
-- we don't. Ugh. ... adding it to the filename, so we can reparse it out.

-- le sigh.

-- Okay, now we need to download these files from the yahoo! REST endpoint
-- then load these files into our database.

deleteCandlesStmt :: Query
deleteCandlesStmt = "DELETE FROM source WHERE for_day=? AND source_type_id=?"

downloadCandlesticks :: Connection -> Day -> IO ()
downloadCandlesticks conn tday =
   lookupTable conn "source_type_lk" >>= \srcs ->
   trackedCoins conn                 >>= \trackedCoins ->
   let ts f = traverse (sequence . f)
       candlesF = ((id *** fst) &&& fetchOCHLV tday) in
   report 0 ("Downloading and storing candlestick files.")
            (execute conn deleteCandlesStmt (tday, cndlstks srcs) >>
             maxOr250 conn tday trackedCoins                      >>=
             ts candlesF . Map.toList                             >>=
             mapM_  (maybeStoreCandlestickCSVFile conn srcs tday))

go :: IO ()
go = geaux downloadCandlesticks
