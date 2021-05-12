{-# LANGUAGE OverloadedStrings #-}

module CryptoCoin.CoinMarketCap.Data.Transaction where

-- Records a transaction for a coin into a portfolio

import Control.Monad ((>=>))
import qualified Data.Map as Map
import Data.Maybe (mapMaybe, catMaybes)
import Database.PostgreSQL.Simple

import Control.Scan.CSV

import Data.CryptoCurrency.Types.Transaction
import Data.LookupTable
import Data.Monetary.USD

import Store.SQL.Connection (withConnection, Database(ECOIN), connectInfo)
import Store.SQL.Util.LookupTable

type StoreTransactionsF =
   Connection -> TransactionContext -> [Transaction] -> IO ()

storeTransactionsAssocRecommendations :: StoreTransactionsF
storeTransactionsAssocRecommendations conn (TC symLk calll portl) =
   mapM (storeTransaction conn symLk calll portl) >=>
   joinRecommendations conn . catMaybes

-- to store just the transactions, use storeTransaction from Types.

onlyStoreTransactions :: StoreTransactionsF
onlyStoreTransactions conn (TC symLk calll portl) =
   mapM_ (storeTransaction conn symLk calll portl)

makeTransaction :: String -> Maybe Transaction
makeTransaction = mkTrans . csv

mkTrans :: [String] -> Maybe Transaction
mkTrans [sym,date,spent,surcharge,amt,call,portfolio] =
   Transaction sym <$> readMaybe date <*> readMaybe spent
                   <*> readMaybe surcharge <*> readMaybe amt
                   <*> readMaybe call <*> Just portfolio

readTransactions :: FilePath -> IO [Transaction]
readTransactions file =
   mapMaybe makeTransaction . tail . lines <$> readFile file

data TransactionContext = TC { symLk, callLk, portfolioLk :: LookupTable }
   deriving (Eq, Ord, Show)

transContext :: Connection -> IO TransactionContext
transContext conn =
   TC <$> lookupTableFrom conn "SELECT cmc_id, symbol FROM coin"
      <*> lookupTable conn "call_lk"
      <*> lookupTableFrom conn
                   "SELECT portfolio_id, portfolio_name FROM portfolio"

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

... we can now look at the transactoins in a report
--}
