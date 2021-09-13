{-# LANGUAGE OverloadedStrings #-}

module CryptoCoin.CoinMarketCap.ETL.TransactionCSVLoader where

{--
We handle staking/earning/reinvesting differently from transactions in a few
ways: 1. we don't transfer out from the linked account, 2. these transactions
are handled differently for tax-purposes, 3. do these reinvestments make these
coins more desirable buys? TUNE IN NEXT WEEK FOR ANSWERS TO THESE QUESTIONS, ...
AND MORE!
--}

import Data.Maybe (mapMaybe)
import Data.Time (Day)

import Database.PostgreSQL.Simple (Connection)

import Control.List (softtail)
import Control.Scan.CSV (readMaybe)

import CryptoCoin.CoinMarketCap.Utils (geaux, dateDir)

import Data.CryptoCurrency.Types.Recommendation (Call(CLAIM))
import Data.CryptoCurrency.Types.Transaction
           (Transaction(Transaction), StoreTransactionsF, storeTransaction)
import Data.CryptoCurrency.Types.Transactions.Context (transContext)
import Data.CryptoCurrency.Utils (report, plural, fileProcessor)

import Data.Monetary.USD (USD(USD))

import Store.SQL.Connection (withConnection, Database(ECOIN), connectInfo)

reinvest :: StoreTransactionsF
reinvest conn tc = mapM_ (storeTransaction conn tc)

nomonay :: Maybe USD
nomonay = Just (USD 0)

mkReinvest :: [String] -> Maybe Transaction
mkReinvest a@[sym,date,amt,portfolio,_confirmation] = mr' nomonay (init a)
mkReinvest a@[sym,date,amt,portfolio] = mr' nomonay a
mkReinvest [sym,date,amt,portfolio,surcharge,_confirmation] =
   mr' (readMaybe surcharge) [sym,date,amt,portfolio]
mkReinvest row = error ("Could not process: " ++ show row)

mr' :: Maybe USD -> [String] -> Maybe Transaction
mr' surcharge [sym,date,amt,portfolio] =
   Transaction sym <$> readMaybe date <*> nomonay <*> surcharge
                   <*> readMaybe amt <*> Just CLAIM <*> Just portfolio

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
go = geaux addAllStakes

addAllStakes :: Connection -> Day -> IO ()
addAllStakes conn date =
  dateDir "stakes" date                             >>=
  fileProcessor mkReinvest . (++ "/reinvested.csv") >>= \rs ->
  report 0 (msg (length rs)) (transContext conn >>= flip (reinvest conn) rs)

msg :: Int -> String
msg su | su == 0 = "Storing no new reinvestments today."
       | otherwise = "Storing " ++ show su ++ " reinvestment" ++ plural su
