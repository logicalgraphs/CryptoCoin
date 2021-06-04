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

import Data.CryptoCurrency.Types.Transfer
import Data.CryptoCurrency.Types.Transfers.Context
import Data.CryptoCurrency.Utils (report, plural)
import Data.Monetary.USD
import Data.Time.TimeSeries (today)

import Store.SQL.Connection (withConnection, Database(ECOIN))

makeTransfer :: String -> Maybe Transfer
makeTransfer = mkTransF . csv

mkTransF :: [String] -> Maybe Transfer
mkTransF [dt, port, dir, amt] =
   Transfer <$> readMaybe dt <*> Just port <*> readMaybe dir <*> readMaybe amt

readTransfers :: FilePath -> IO [Transfer]
readTransfers file = doesFileExist file >>= rt' file

rt' :: FilePath -> Bool -> IO [Transfer]
rt' _ False = return []
rt' file True =
   mapMaybe makeTransfer . mbtail . lines <$> readFile file
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
>>> transfers <- readTransfers (cr ++ "/data-files/transfers/2021-06-03/transfers.csv")

>>> transfers
[Transfer {dt = 2021-06-03, port = "USAA", dir = INCOME, amt = $2000.00},
 Transfer {dt = 2021-06-03, port = "USAA", dir = OUTGO, amt = $800.00},
 Transfer {dt = 2021-06-03, port = "GEMINI", dir = INCOME, amt = $800.00}]

So! We want to store these transfers, then group them by portfolio, then update
each portfolio's cash reserve.
--}

go :: IO ()
go = today >>= \tday ->
     withConnection ECOIN (flip storeTransfersAndUpdatePortfolii tday)

storeTransfersAndUpdatePortfolii :: Connection -> Day -> IO ()
storeTransfersAndUpdatePortfolii conn date =
  getEnv "CRYPTOCOIN_DIR"                                      >>= \ccd ->
  let dataDir = ccd ++ "/data-files/transfers/" ++ show date in
  readTransfers (dataDir ++ "/transfers.csv")                  >>= \xfers ->
  report 0 (msg (length xfers)) (transFContext conn            >>= \tfc ->
                                 storeTransfers conn tfc xfers >>
                                 updatePortfolii conn tfc xfers)

msg :: Int -> String
msg su | su == 0 = "Storing no new transfers today."
       | otherwise = "Storing " ++ show su ++ " transfer" ++ plural su

updatePortfolii :: StoreTransferF
updatePortfolii conn tfc =
   mapM_ (updateCashReserves conn tfc . Set.toList)
       . Map.elems
       . snarf (pure . (port &&& id))
