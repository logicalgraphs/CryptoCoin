{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module CryptoCoin.CoinMarketCap.ETL.TransactionCSVLoader where

-- Records a transaction for a coin into a portfolio

import Control.Monad ((>=>))
import qualified Data.Map as Map
import Data.Maybe (mapMaybe, catMaybes)
import Database.PostgreSQL.Simple
import System.Directory (doesFileExist)
import System.Environment (getEnv)

import Control.Scan.CSV


import Data.CryptoCurrency.Types.Coin (allCoinsLk)
import Data.CryptoCurrency.Types.Portfolio (portfoliiLk)
import Data.CryptoCurrency.Types.Transaction
import Data.CryptoCurrency.Types.TransactionContext
import Data.CryptoCurrency.Utils (report, conj, plural)
import Data.Monetary.USD
import Data.Time.TimeSeries (today)

import Store.SQL.Connection (withConnection, Database(ECOIN), connectInfo)

type StoreTransactionsF =
   Connection -> TransactionContext -> [Transaction] -> IO ()

storeTransactionsAssocRecommendations :: StoreTransactionsF
storeTransactionsAssocRecommendations conn tc =
   mapM_ (storeTransaction conn tc >=> joinRecommendations conn)

-- to store just the transactions, use storeTransaction from Types.

onlyStoreTransactions :: StoreTransactionsF
onlyStoreTransactions conn tc = mapM_ (storeTransaction conn tc)

makeTransaction :: String -> Maybe Transaction
makeTransaction = mkTrans . csv

mkTrans :: [String] -> Maybe Transaction
mkTrans [sym,date,spent,surcharge,amt,call,portfolio,_confirmation] =
   mkTrans [sym,date,spent,surcharge,amt,call,portfolio]
mkTrans [sym,date,spent,surcharge,amt,call,portfolio] =
   Transaction sym <$> readMaybe date <*> readMaybe spent
                   <*> readMaybe surcharge <*> readMaybe amt
                   <*> readMaybe call <*> Just portfolio

readTransactions :: FilePath -> IO [Transaction]
readTransactions file =
   doesFileExist file >>= rt' file

rt' :: FilePath -> Bool -> IO [Transaction]
rt' _ False = return []
rt' file True =
   mapMaybe makeTransaction . mbtail . lines <$> readFile file
      where mbtail [] = []
            mbtail (_:t) = t

{--
>>> withConnection ECOIN (\conn -> transContext conn >>= \(TC cns a b) ->
       putStrLn "Coins" >> mapM_ print (take 5 (Map.toList cns)) >>
       putStrLn "Calls" >> mapM_ print (Map.toList a) >> 
       putStrLn "Portfolii" >> mapM_ print (Map.toList b))
Coins
("#MetaHash",3756)
("$YEET",7565)
("01coin",4546)
("0Chain",2882)
("0cash",5316)

Calls
("BUY",1)
("SELL",2)

Portfolii
("BINANCE",2)
("COINBASE",1)

And with that, we can now do, e.g.:

>>> transs <- readTransactions "holdings.csv" 
>>> withConnection ECOIN (\conn ->
         transContext conn >>= 
         flip (onlyStoreTransactions conn) transs)

... we can now look at the transactions in a report
--}

-- so, to load all the transactions for today:

{--
The data-files are in the format:

sym,date,spent,surcharge,amt,call,portfolio
BTC,2021-02-18,$0.00,$0.00,0.00009599,BUY,COINBASE
XLM,2021-04-26,$0.00,$0.00,4.0871962,BUY,COINBASE

and are named not-recommended.csv and recommended.csv
--}

go :: IO ()
go = today                                                >>= \tday ->
     getEnv "CRYPTOCOIN_DIR"                              >>= \ccd ->
     let dataDir = ccd ++ "/data-files/transactions/" ++ show tday in
     readTransactions (dataDir ++ "/recommended.csv")     >>= \recs ->
     readTransactions (dataDir ++ "/not-recommended.csv") >>= \nrecs ->
     report 0 (msg recs nrecs)
            (withConnection ECOIN (\conn ->
               transContext conn                          >>= \tc ->
               onlyStoreTransactions conn tc nrecs        >>
               storeTransactionsAssocRecommendations conn tc recs))

msg :: [a] -> [b] -> String
msg (length -> recs) (length -> nonrecs) = msg' recs nonrecs (recs + nonrecs)

msg' :: Int -> Int -> Int -> String
msg' r n su | su == 0 = "Storing no new transactions today."
            | otherwise = "Storing" ++ rekts r "" ++ conj r n "and"
                       ++ rekts n "non-"

rekts :: Int -> String -> String
rekts sz kind | sz == 0   = ""
              | otherwise = ' ':show sz ++ " " ++ kind ++ "recommended-"
                            ++ "transaction" ++ plural sz
