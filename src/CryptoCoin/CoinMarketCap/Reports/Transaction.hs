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

import qualified Data.Map as Map
import Data.Time (Day)

import Database.PostgreSQL.Simple (Connection)

import Control.List (weave)
import Control.Presentation hiding (S)

import Data.CryptoCurrency.Types.Transaction
import Data.CryptoCurrency.Types.Transactions.Context

import Data.Time.TimeSeries (today)

import Data.XHTML hiding (nb)

import Store.SQL.Connection (withConnection, Database(ECOIN))

elt :: String -> String -> Content
elt e = E . Elt e [] . pure . S

p :: String -> Content
p = elt "p"

addendum :: [Content]
addendum = [p nb]

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

printTransactions :: Day -> (CoinTransactions, CoinTransactions) -> IO ()
printTransactions date (recs, nonrecs) =
   let rpt = [p (header date),
              E (Elt "ol" [] (transactionRows listItem recs nonrecs)),
              p "That's it for today."]
       addend = if length nonrecs == 0 then id else (++ addendum)
   in printContent (E (Elt "p" [] (addend rpt))) 0

printCSVTransactions :: Day -> (CoinTransactions, CoinTransactions) -> IO ()
printCSVTransactions date (recs, nonrecs) =
   let rpt = [header date, ""] ++ transactionRows csvRow recs nonrecs ++
             ["", "That's it for today."]
       addend = if length nonrecs == 0 then id else (++ ["", nb])
   in  putStrLn (unlines (addend rpt))

listItem :: Bool -> Transaction -> Content
listItem withStar (Transaction sym _dt amt _surcharge coins call portfolio) =
   let star = if withStar then "*" else "" in
   elt "li" (unwords [show call, show amt, sym ++ ",", portfolio, star])

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
go = today >>= withConnection ECOIN . flip reportTransactions

{--
As HTML:

>>> withConnection ECOIN (flip reportTransactions (read "2021-05-21"))
<p>
 <p>
  I am making the following transactions for 2021-05-21
 </p>
 <ol>
  <li>
   BUY $100.00 BTC, GEMINI
  </li>
  <li>
   BUY $100.00 LTC, GEMINI
  </li>
...
  <li>
   BUY $100.00 ETH, GEMINI *
  </li>
 </ol>
 <p>
  That's it for today.
 </p>
 <p>
  * There were no recommendations for these two transactions; I bought these coins on sale, is all.
 </p>
</p>

As CSV:

I am making the following transactions for 2021-05-21

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

* There were no recommendations for these two transactions; I bought these coins on sale, is all.
--}
