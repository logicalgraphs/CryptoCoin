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

import Data.CryptoCurrency.Types.Transaction
import Data.CryptoCurrency.Types.Transactions.Context

import Data.Time.TimeSeries (today)

import Data.XHTML

import Store.SQL.Connection (withConnection, Database(ECOIN))

elt :: String -> String -> Content
elt e = E . Elt e [] . pure . S

p :: String -> Content
p = elt "p"

addendum :: [Content]
addendum = [p ("* There were no recommendations for these two transactions; I "
               ++ "bought these coins on sale, is all.")]

printTransactions :: Day -> (CoinTransactions, CoinTransactions) -> IO ()
printTransactions date (recs, nonrecs) =
   let ce = concat . Map.elems
       rpt = [p ("I am making the following transactions for " ++ show date),
              E (Elt "ol" [] (map (listItem True) (ce nonrecs)
                           ++ map (listItem False) (ce recs))),
              p "That's it for today."]
       addend = if length nonrecs == 0 then id else (++ addendum)
   in printContent (E (Elt "p" [] (addend rpt))) 0

listItem :: Bool -> Transaction -> Content
listItem withStar (Transaction sym _dt amt _surcharge coins call portfolio) =
   let star = if withStar then "*" else "" in
   elt "li" (unwords [show call, show amt, sym ++ ",", portfolio, star])

reportTransactions :: Connection -> Day -> IO ()
reportTransactions conn tday =
   transContext conn                        >>=
   flip (fetchTransactionsByDate conn) tday >>=
   printTransactions tday

go :: IO ()
go = today >>= withConnection ECOIN . flip reportTransactions
