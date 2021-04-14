module CryptoCoin.CoinMarketCap.Go where

-- runs everything: uploads files, ETLs them, runs the report

-- a propos de rein: the function go in the module Go is pronounced "Go-go."
-- #KillBill, by Quentin Tarentino
-- https://en.wikipedia.org/wiki/The_Bride_(Kill_Bill)

import Control.Arrow ((&&&))

import Data.Time (Day)

import Database.PostgreSQL.Simple

import CryptoCoin.CoinMarketCap.Types (NewCoinsCtx)

import CryptoCoin.CoinMarketCap.Analytics.Candlesticks.Patterns (candlesAll)
import CryptoCoin.CoinMarketCap.Analytics.Trends.Trend (storeTrends)

import CryptoCoin.CoinMarketCap.Data.TrackedCoin (trackedCoins)

import CryptoCoin.CoinMarketCap.ETL.JSONFile (extractListings)
import CryptoCoin.CoinMarketCap.ETL.NewCoinLoader (processFiles, newCoins)
import CryptoCoin.CoinMarketCap.ETL.SourceFileLoader (uploadFiles)
import CryptoCoin.CoinMarketCap.ETL.Candlesticks.Loader (downloadCandlesticks)
import CryptoCoin.CoinMarketCap.ETL.Candlesticks.Transformer (processAllCandlesticks)

import CryptoCoin.CoinMarketCap.Reports.Reporter (ranking, tweet, title)

import Data.LookupTable (LookupTable)
import Data.Time.TimeSeries (today)

import Store.SQL.Connection (withConnection, Database(ECOIN))
import Store.SQL.Util.Indexed (val)
import Store.SQL.Util.LookupTable (lookupTable)

go :: IO ()
go = withECoinReport (\conn srcs currs trackeds tday ->
        uploadFiles conn srcs                        >>
        downloadCandlesticks conn srcs trackeds      >>
        processAllCandlesticks conn srcs currs       >>
        candlesAll conn trackeds                     >>
        processFiles conn srcs                       >>=
        pass (storeTrends conn tday trackeds))

pass :: IO a -> b -> IO b
pass proc ans = proc >> return ans

-- if we want just the report (because we did the upload, but then SOMEbody
-- messed up a database-insert and that was done, later, manually) ...

report :: IO ()
report = withECoinReport (\conn srcs _currencies trackeds _tday ->
   candlesAll conn trackeds  >>
   extractListings conn srcs >>=
   traverse (sequence . (id &&& newCoins conn . val)))

type Proc = Connection -> LookupTable -> LookupTable -> LookupTable -> Day
         -> IO [NewCoinsCtx]

withECoinReport :: Proc -> IO ()
withECoinReport proc =
   today                                   >>= \tday ->
   withConnection ECOIN                       (\conn ->
      lookupTable conn "source_type_lk"    >>= \srcLk ->
      lookupTable conn "currency_lk"       >>= \currLk ->
      trackedCoins conn                    >>= \trackeds ->
      proc conn srcLk currLk trackeds tday >>=
      mapM_ (\listings -> ranking tday listings >>= tweet tday >> title tday))
