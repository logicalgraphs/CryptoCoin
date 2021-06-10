{-# LANGUAGE ViewPatterns #-}

module CryptoCoin.CoinMarketCap.Reports.Transaction where

{--
We want our transaction report to look something like this:

-----
I am making the following transactions for 2021-05-19:

One-time transactions today:

1. $100 Cardano ADA, Coinbase *
2. $100 Ethereum ETH, Coinbase *
3. $100 The Graph GRT, Coinbase
4. $100 Enjin ENJ, Coinbase
5. $100 Cosmos ATOM, Coinbase
6. $100 Litecoin LTC, Coinbase
7. $100 Bitcoin BTC, Coinbase

That's it for today.

* There were no recommendations for these two transactions; I bought these 
coins on sale, is all.
-----

This implies we have transactions (today) that are recommended and ones
that are not. So, we need to find this out from the join table: DONE in
the transaction-pull.
--}

import Control.Arrow ((***))
import Control.Monad (join)

import qualified Data.Map as Map
import Data.Time (Day)

import Database.PostgreSQL.Simple (Connection)

import Control.List (weave)
import Control.Presentation hiding (S)

import Data.CryptoCurrency.Types.Transaction
import Data.CryptoCurrency.Types.Transactions.Context
import Data.Monetary.USD

import Store.SQL.Connection (withConnection, Database(ECOIN))
import Store.SQL.Util.Time (latest)

nb :: String
nb = "* There were no recommendations for these transactions; I "
   ++ "bought these coins on sale, is all."

header :: Day -> String
header date = show date ++ ",I am making the following transactions"

mapper :: (Bool -> Transaction -> row) -> Bool -> CoinTransactions -> [row]
mapper f b = map (f b) . concat . Map.elems

transactionRows :: (Bool -> Transaction -> row)
                -> CoinTransactions -> CoinTransactions -> [row]
transactionRows r (mapper r False -> rs) (mapper r True -> nrs) = rs ++ nrs

printCSVTransactions :: Day -> (CoinTransactions, CoinTransactions) -> IO ()
printCSVTransactions date (join (***) filterOut -> (recs, nonrecs)) =
   let rpt = (header date:lf "call,amount,coin,exchange")
             ++ transactionRows csvRow recs nonrecs
             ++ lf "That's it for today."
       addend = if length nonrecs == 0 then id else (++ lf nb)
   in  putStrLn (unlines (addend rpt))

filterOut :: CoinTransactions -> CoinTransactions
filterOut = Map.map (filter ((> USD 9) . spent))

lf :: String -> [String]
lf = ("":) . pure

csvRow :: Bool -> Transaction -> String
csvRow withStar (Transaction sym _dt amt _surcharge coins call portfolio) =
   let star = if withStar then (++ " *") else id in
   weave [show call, show amt, sym, star portfolio]

reportTransactions :: Connection -> Day -> IO ()
reportTransactions conn tday =
   transContext conn                        >>=
   flip (fetchTransactionsByDate conn) tday >>=
   printCSVTransactions tday

go :: IO ()
go = withConnection ECOIN (\conn ->
       latest conn "transaction_log" "for_date" >>=
       reportTransactions conn)

{--
As CSV:

2021-05-21,I am making the following transactions

call,amount,coin,exchange
BUY,$100.00,BTC,GEMINI
BUY,$100.00,LTC,GEMINI
BUY,$100.00,XTZ,COINBASE
BUY,$100.00,ATOM,COINBASE
BUY,$100.00,ALGO,COINBASE
BUY,$100.00,UMA,GEMINI
BUY,$100.00,GRT,GEMINI
BUY,$100.00,1INCH,GEMINI
BUY,$100.00,1INCH,GEMINI
BUY,$100.00,DOGE,GEMINI *
BUY,$100.00,ETH,GEMINI *

That's it for today.

* There were no recommendations for these transactions; I bought these coins on sale, is all.
--}
