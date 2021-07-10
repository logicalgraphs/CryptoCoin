{-# LANGUAGE ViewPatterns #-}

module CryptoCoin.CoinMarketCap.ETL.TransferCSVLoader where

-- Transfer funds into and out of portfolii.

import Control.Arrow ((&&&))

import qualified Data.Map as Map
import Data.Maybe (mapMaybe, catMaybes)
import qualified Data.Set as Set
import Data.Time (Day)

import Database.PostgreSQL.Simple

import System.Directory (doesFileExist)
import System.Environment (getEnv)

import Control.Map (snarf)
import Control.Scan.CSV

import CryptoCoin.CoinMarketCap.Utils (geaux)

import Data.CryptoCurrency.Types.Transfer
import Data.CryptoCurrency.Types.Transfers.Context
import Data.CryptoCurrency.Utils (report, plural)
import Data.Monetary.USD
import Data.Time.TimeSeries (today)

import Store.SQL.Connection (withConnection, Database(ECOIN))

makeCashTransfer :: String -> Maybe CashTransfer
makeCashTransfer = mkCashTransF . csv

mkCashTransF :: [String] -> Maybe CashTransfer
mkCashTransF [readMaybe -> dt, port, readMaybe -> dir, readMaybe -> amt] =
   CashTransfer <$> dt <*> Just port <*> dir <*> amt

readCashTransfers :: FilePath -> IO [CashTransfer]
readCashTransfers file = doesFileExist file >>= rct' file

rct' :: FilePath -> Bool -> IO [CashTransfer]
rct' _ False = return []
rct' file True =
   mapMaybe makeCashTransfer . mbtail . lines <$> readFile file
      where mbtail [] = []
            mbtail (_:t) = t

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
go = geaux storeTransfersAndUpdatePortfoliiCSV

storeTransfersAndUpdatePortfoliiCSV :: Connection -> Day -> IO ()
storeTransfersAndUpdatePortfoliiCSV conn date =
   getEnv "CRYPTOCOIN_DIR"                                      >>= \ccd ->
   let dataDir = ccd ++ "/data-files/transfers/" ++ show date in
   readCashTransfers (dataDir ++ "/cash.csv")                   >>=
   storeTransfersAndUpdatePortfolii conn
