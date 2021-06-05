{-# LANGUAGE OverloadedStrings #-}

module CryptoCoin.CoinMarketCap.ETL.TransactionCSVLoader where

{--
We handle staking/earning/reinvesting differently from transactions in a few
ways: 1. we don't transfer out from the linked account, 2. these transactions
are handled differently for tax-purposes, 3. do these reinvestments make these
coins more desirable buys? TUNE IN NEXT WEEK FOR ANSWERS TO THESE QUESTIONS, ...
AND MORE!
--}

{--
import Control.Monad ((>=>))
import qualified Data.Map as Map
import Data.LookupTable
--}

import Data.Maybe (mapMaybe)
import Data.Time (Day)

import Database.PostgreSQL.Simple (Connection)

import System.Directory (doesFileExist)
import System.Environment (getEnv)

import Control.Scan.CSV (csv, readMaybe)
import Data.CryptoCurrency.Types.Recommendation (Call(BUY))
import Data.CryptoCurrency.Types.Transaction
import Data.CryptoCurrency.Types.Transactions.Context
import Data.CryptoCurrency.Utils (report, plural)

import Data.Monetary.USD
import Data.Time.TimeSeries (today)

import Store.SQL.Connection (withConnection, Database(ECOIN), connectInfo)

reinvest :: StoreTransactionsF
reinvest conn tc = mapM_ (storeTransaction conn tc)

mkReinvest :: [String] -> Maybe Transaction
mkReinvest [sym,date,amt,portfolio,_confirmation] =
   mkReinvest [sym,date,amt,portfolio]
mkReinvest [sym,date,amt,portfolio] =
   let nomonay = Just (USD 0) in
   Transaction sym <$> readMaybe date <*> nomonay <*> nomonay
                   <*> readMaybe amt <*> Just BUY <*> Just portfolio

readReinvestments :: FilePath -> IO [Transaction]
readReinvestments file = doesFileExist file >>= rr' file

rr' :: FilePath -> Bool -> IO [Transaction]
rr' _ False = return []
rr' file True =
   mapMaybe (mkReinvest . csv) . mbtail . lines <$> readFile file
      where mbtail [] = []
            mbtail (_:t) = t

{--
>>> transs <- readTransactions "holdings.csv" 
>>> withConnection ECOIN (\conn ->
         transContext conn >>= 
         flip (onlyStoreTransactions conn) transs)

... we can now look at the transactions in a report
--}

-- so, to load all the transactions for today:

{--
The data-files are in the format:

sym,date,amt,portfolio
BTC,2021-02-18,0.00009599,COINBASE
XLM,2021-04-26,4.0871962,COINBASE

and are named reinvested.csv
--}

go :: IO ()
go = today >>= withConnection ECOIN . flip addAllStakes

addAllStakes :: Connection -> Day -> IO ()
addAllStakes conn date =
  getEnv "CRYPTOCOIN_DIR"                                    >>= \ccd ->
  let dataDir = ccd ++ "/data-files/stakes/" ++ show date in
  readReinvestments (dataDir ++ "/reinvested.csv")           >>= \reinvs ->
  report 0 (msg (length reinvs))
         (transContext conn >>= flip (reinvest conn) reinvs)

msg :: Int -> String
msg su | su == 0 = "Storing no new reinvestments today."
       | otherwise = "Storing " ++ show su ++ " reinvestment" ++ plural su
