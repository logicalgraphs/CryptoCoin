{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Data.CryptoCurrency.Types.Transfer where

{--
A transfer IS A movement of funds from one portfolio to another. It HAS A

   portfolio (id) (that we get from the portfolio name and portfolio map)
   direction (IN or OUT)
   and an amount (USD) transferred.

WHEN a transfer (actually an income or an outgo) occurs the portfolio's cash
reserve is adjusted by that amount.

SO! If I do a transaction (you heard me correctly) on gemini, I transfer $100
out of my USAA portfolio and then (also) transfer $100 into my gemini portfolio
to effect the transaction.

This way, I can stay on top of my cash reserves.
--}

import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Time (Day)

import Database.PostgreSQL.Simple

import Data.CryptoCurrency.Types hiding (idx)
import Data.CryptoCurrency.Types.Portfolio
import Data.CryptoCurrency.Types.Transfers.Context
import Data.CryptoCurrency.Types.Transfers.Internal
import Data.CryptoCurrency.Utils (report)
import Data.Monetary.USD
import Data.XHTML (Name)

import Store.SQL.Util.LookupTable
import Store.SQL.Util.Indexed (IxValue, idx, val)

data Direction = INCOME | OUTGO
   deriving (Eq, Ord, Show, Read)

inorout :: Direction -> Double
inorout INCOME = 1
inorout OUTGO = (-1)

data Transfer =
   Transfer { dt :: Day, port :: Name, dir ::  Direction, amt :: USD }
     deriving (Eq, Ord, Show)

-- STORE FUNCTIONS -------------------------------------------------------

toTransF' :: TransferContext -> Transfer -> Maybe TransF'
toTransF' (TfC dirLk ports) (Transfer dt p dir amt) =
   Tf dt amt <$> (idx <$> lk (show p) ports) <*> lk (show dir) dirLk
      where lk = Map.lookup

type StoreTransferF = Connection -> TransferContext -> [Transfer] -> IO ()

storeTransfers :: StoreTransferF
storeTransfers conn ctx = storeTransF' conn . mapMaybe (toTransF' ctx)

toAmt :: Transfer -> Double
toAmt = (*) . doubledown . amt <*> inorout . dir

updateCashReserveQuery :: Query
updateCashReserveQuery = "UPDATE portfolio SET cash=? WHERE portfolio_id=?"

updateCashReserves :: StoreTransferF
updateCashReserves conn tfc xfers =
   let adj = sum (map toAmt xfers)
       portName = port (head xfers)
       mbIxPort = Map.lookup portName (portfolii tfc)
       showD = show . USD . toRational in
   maybe (putStrLn ("No portfolio named " ++ portName))
         (\ip -> let newcash = adj + doubledown (cash (val ip))
                     ix = idx ip in
                 report 0 (unwords ["Adjusting",portfolioName (val ip),"by",
                                    showD adj, "to", showD newcash])
                          (execute conn updateCashReserveQuery (ix,newcash)))
         mbIxPort
