module CryptoCoin.CoinMarketCap.Go where

-- runs everything: uploads files, ETLs them, runs the report

-- a propos de rein: the function go in the module Go is pronounced "Go-go."
-- #KillBill, by Quentin Tarentino
-- https://en.wikipedia.org/wiki/The_Bride_(Kill_Bill)

import Database.PostgreSQL.Simple

import CryptoCoin.CoinMarketCap.ETL.SourceFileLoader (uploadFiles)
import CryptoCoin.CoinMarketCap.ETL.NewCoinLoader (processFiles)
import CryptoCoin.CoinMarketCap.Reports.Reporter

import Data.Time.TimeSeries (today)

import Store.SQL.Connection
import Store.SQL.Util.LookupTable

go :: IO ()
go = 
   today                                >>= \tday ->
   withConnection ECOIN                    (\conn ->
      lookupTable conn "source_type_lk" >>= \srcs ->
      uploadFiles conn srcs             >>
      processFiles conn srcs            >>=
      mapM_ (\listings -> ranking tday listings >>= tweet tday >> title tday))
