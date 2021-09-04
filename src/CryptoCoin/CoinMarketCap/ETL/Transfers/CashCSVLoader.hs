{-# LANGUAGE ViewPatterns #-}

module CryptoCoin.CoinMarketCap.ETL.Transfers.CashCSVLoader where

-- Transfer funds into and out of portfolii.

import Control.Arrow ((&&&))

import qualified Data.Map as Map
import Data.Maybe (mapMaybe, catMaybes)
import qualified Data.Set as Set
import Data.Time (Day)

import Database.PostgreSQL.Simple (Connection)

import Control.Map (snarf)
import Control.Scan.CSV (readMaybe)

import CryptoCoin.CoinMarketCap.Utils (geaux, dateDir)

import Data.CryptoCurrency.Types.Transfers.Cash
           (CashTransfer(CashTransfer), storeCashTransfersAndUpdatePortfolii)
import Data.CryptoCurrency.Utils (report, plural, fileProcessor)

import Store.SQL.Connection (withConnection, Database(ECOIN))

mkCashTransF :: [String] -> Maybe CashTransfer
mkCashTransF [readMaybe -> dt, port, readMaybe -> dir, readMaybe -> amt] =
   CashTransfer <$> dt <*> Just port <*> dir <*> amt

{--
>>> withConnection ECOIN (\conn -> transFContext conn >>= print)
TfC {dirLk = {("INCOME",1),("OUTGO",2)}, 
     portfolii = {("BINANCE",
                   IxV {ix = 2,
                        val = Portfolio {portfolioName = "BINANCE", 
                                         cash = $0.00,
                                         raison = Just "BINANCE"}}),
                  ...,
                  ("USAA",
                   IxV {ix = 4, 
                        val = Portfolio {portfolioName = "USAA",
                                         cash = $-81.59
                                         raison = Just "BANK_ACCOUNT"}})]}

The data-files are in the format:

date,portfolio,direction,amount
2021-06-03,USAA,INCOME,$2000.00
2021-06-03,USAA,OUTGO,$800
2021-06-03,GEMINI,INCOME,$800

and are named transfers.csv

And with that, we can now do, e.g.:

>>> cr <- getEnv "CRYPTOCOIN_DIR"
>>> transfers <- readCashTransfers (cr ++ "/data-files/transfers/2021-06-03/cash.csv")

>>> transfers
[CashTransfer {dt = 2021-06-03, port = "USAA", dir = INCOME, amt = $2000.00},
 CashTransfer {dt = 2021-06-03, port = "USAA", dir = OUTGO, amt = $800.00},
 CashTransfer {dt = 2021-06-03, port = "GEMINI", dir = INCOME, amt = $800.00}]

So! We want to store these transfers, then group them by portfolio, then update
each portfolio's cash reserve.
--}

go :: IO ()
go = geaux storeCashTransfersAndUpdatePortfoliiCSV

storeCashTransfersAndUpdatePortfoliiCSV :: Connection -> Day -> IO ()
storeCashTransfersAndUpdatePortfoliiCSV conn date =
   dateDir "transfers" date                      >>=
   fileProcessor mkCashTransF . (++ "/cash.csv") >>=
   storeCashTransfersAndUpdatePortfolii conn
