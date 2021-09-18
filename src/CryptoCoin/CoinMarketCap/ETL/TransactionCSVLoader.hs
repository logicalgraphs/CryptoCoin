{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module CryptoCoin.CoinMarketCap.ETL.TransactionCSVLoader where

-- Records a transaction for a coin into a portfolio

import Control.Monad ((>=>))

import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Time (Day)

import Database.PostgreSQL.Simple (Connection)

import Control.Scan.CSV (readMaybe)

import CryptoCoin.CoinMarketCap.Utils (geaux, gon, dateDir)

import Data.CryptoCurrency.Types.Recommendation (Call(BUY, SELL))
import Data.CryptoCurrency.Types.Transaction
           (StoreTransactionsF, storeTransaction, Transaction(Transaction),
            joinRecommendations)
import Data.CryptoCurrency.Types.Transactions.Context
           (TransactionContext(TaC), transContext)
import Data.CryptoCurrency.Types.Transfers.Cash
           (CashTransfer(CashTransfer), Direction(OUTGO, INCOME),
            storeCashTransfersAndUpdatePortfolii)
import Data.CryptoCurrency.Utils (report, conj, plural, fileProcessor)
import Data.LookupTable (LookDown, lookdown)
import Data.Monetary.USD (USD(USD))

import Store.SQL.Connection (withConnection, Database(ECOIN), connectInfo)

storeTransactionsAssocRecommendations :: StoreTransactionsF
storeTransactionsAssocRecommendations conn tc =
   mapM_ (storeTransaction conn tc >=> joinRecommendations conn)

-- to store just the transactions, use storeTransaction from Types.

onlyStoreTransactions :: StoreTransactionsF
onlyStoreTransactions conn tc = mapM_ (storeTransaction conn tc)

mkTrans :: [String] -> Maybe Transaction
mkTrans [sym,date,spent,surcharge,amt,call,portfolio,_confirmation] =
   mkTrans [sym,date,spent,surcharge,amt,call,portfolio]
mkTrans [sym,date,spent,surcharge,amt,call,portfolio] =
   Transaction sym <$> readMaybe date <*> readMaybe spent
                   <*> readMaybe surcharge <*> readMaybe amt
                   <*> readMaybe call <*> Just portfolio

readTransactions :: FilePath -> IO [Transaction]
readTransactions = fileProcessor mkTrans

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
go = geaux addAllTransactions

addAllTransactions :: Connection -> Day -> IO ()
addAllTransactions conn date =
  dateDir "transactions" date                                >>= \dataDir ->
  readTransactions (dataDir ++ "/recommended.csv")           >>= \recs ->
  readTransactions (dataDir ++ "/not-recommended.csv")       >>= \nrecs ->
  report 0 (msg recs nrecs)
         (transContext conn       >>= \tc@(TaC _ _ portLk _) ->
          let portNames = lookdown portLk
              transfers = mapMaybe (reifyFrom tc portNames) (recs ++ nrecs) in
          onlyStoreTransactions conn tc nrecs                >>
          storeTransactionsAssocRecommendations conn tc recs >>
          storeCashTransfersAndUpdatePortfolii conn transfers)

-- for buys, we transfer in from out linked account. For sells, we 'transfer'
-- to our own account's cash-reserve

reifyFrom :: TransactionContext -> LookDown String -> Transaction
          -> Maybe CashTransfer
reifyFrom (TaC _ _ portLk links) portNames
          (Transaction _ dt amt _ _ BUY port) =
   Map.lookup port portLk    >>=
   flip Map.lookup links     >>=
   flip Map.lookup portNames >>= \p ->
   return (CashTransfer dt p OUTGO amt)
reifyFrom _ _ (Transaction _ dt (USD amt) (USD charge) _ SELL port) =
   pure (CashTransfer dt port INCOME (USD (amt - charge)))

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
