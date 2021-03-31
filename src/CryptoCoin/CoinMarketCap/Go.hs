module CryptoCoin.CoinMarketCap.Go where

-- runs everything: uploads files, ETLs them, runs the report

-- a propos de rein: the function go in the module Go is pronounced "Go-go."
-- #KillBill, by Quentin Tarentino
-- https://en.wikipedia.org/wiki/The_Bride_(Kill_Bill)

import Control.Arrow ((&&&))
import Database.PostgreSQL.Simple

import CryptoCoin.CoinMarketCap.Types (NewCoinsCtx)
import CryptoCoin.CoinMarketCap.ETL.JSONFile (extractListings)
import CryptoCoin.CoinMarketCap.ETL.NewCoinLoader (processFiles, newCoins)
import CryptoCoin.CoinMarketCap.ETL.SourceFileLoader (uploadFiles)
import CryptoCoin.CoinMarketCap.ETL.CandlestickLoader (storeAllCandlesticks)
import CryptoCoin.CoinMarketCap.Reports.Reporter (ranking, tweet, title)

import Data.LookupTable (LookupTable)
import Data.Time.TimeSeries (today)

import Store.SQL.Connection (withConnection, Database(ECOIN))
import Store.SQL.Util.Indexed (val)
import Store.SQL.Util.LookupTable (lookupTable)

go :: IO ()
go = withECoinReport (\conn srcs currs ->
        uploadFiles conn srcs                >>
        storeAllCandlesticks conn srcs currs >>
        processFiles conn srcs)

-- if we want just the report (because we did the upload, but then SOMEbody
-- messed up a database-insert and that was done, later, manually) ...

report :: IO ()
report = withECoinReport (\conn srcs _currencies ->
   extractListings conn srcs >>=
   traverse (sequence . (id &&& newCoins conn . val)))

type Proc = Connection -> LookupTable -> LookupTable -> IO [NewCoinsCtx]

withECoinReport :: Proc -> IO ()
withECoinReport proc =
   today                                >>= \tday ->
   withConnection ECOIN                    (\conn ->
      lookupTable conn "source_type_lk" >>= \srcLk ->
      lookupTable conn "currency_lk"    >>=
      proc conn srcLk                   >>=
      mapM_ (\listings -> ranking tday listings >>= tweet tday >> title tday))
