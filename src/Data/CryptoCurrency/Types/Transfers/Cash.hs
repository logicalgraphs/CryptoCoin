{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Data.CryptoCurrency.Types.Transfers.Cash where

{--
A transfer IS A movement of funds from one portfolio to another. It HAS A

   portfolio (id) (that we get from the portfolio name and portfolio map)
   direction (IN or OUT)
   and an amount (USD) transferred.

WHEN a transfer (actually an income or an outgo) occurs, the portfolio's cash
reserve is adjusted by that amount.

SO! If I do a transaction (you heard me correctly) on gemini, I transfer $100
out of my USAA portfolio to cover the gemini coin purchase.

This way, I can stay on top of my cash reserves.
--}

import Control.Arrow ((&&&))

import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Time (Day)

import Database.PostgreSQL.Simple (Connection, Query, execute)

import Control.Map (snarfL)

import Data.CryptoCurrency.Types.Portfolio (cash, portfolioName)
import Data.CryptoCurrency.Types.Transfers.Context
           (transFContext, TransferFundsContext(TfC), portfolii)
import Data.CryptoCurrency.Types.Transfers.Internal
           (CashTransF'(Cshxf), storeCashTransF', msg)
import Data.CryptoCurrency.Utils (report)
import Data.Monetary.USD (USD(USD), doubledown)
import Data.XHTML (Name)

import Store.SQL.Util.Indexed (idx, val)

data Direction = INCOME | OUTGO
   deriving (Eq, Ord, Show, Read)

inorout :: Direction -> Double
inorout INCOME = 1
inorout OUTGO = (-1)

data CashTransfer =
   CashTransfer { dt :: Day, port :: Name, dir ::  Direction, amt :: USD }
     deriving (Eq, Ord, Show)

storeCashTransfersAndUpdatePortfolii :: Connection -> [CashTransfer] -> IO ()
storeCashTransfersAndUpdatePortfolii conn xfers =
   report 0 (msg (length xfers))
          (transFContext conn            >>= \tfc ->
           storeCashTransfers conn tfc xfers >>
           updatePortfolii conn tfc xfers)

-- INTERNAL FUNCTIONS -------------------------------------------------------

toCashTransF' :: TransferFundsContext -> CashTransfer -> Maybe CashTransF'
toCashTransF' (TfC dirLk ports) (CashTransfer dt p dir amt) =
   Cshxf dt amt <$> (idx <$> lk p ports) <*> lk (show dir) dirLk
      where lk = Map.lookup

type StoreCashTransferF = Connection -> TransferFundsContext -> [CashTransfer]
                       -> IO ()

storeCashTransfers :: StoreCashTransferF
storeCashTransfers conn ctx =
   storeCashTransF' conn . mapMaybe (toCashTransF' ctx)

toAmt :: CashTransfer -> Double
toAmt = (*) . doubledown . amt <*> inorout . dir

updateCashReserveQuery :: Query
updateCashReserveQuery = "UPDATE portfolio SET cash=? WHERE portfolio_id=?"

updateCashReserves :: StoreCashTransferF
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
                          (execute conn updateCashReserveQuery (newcash,ix)))
         mbIxPort

updatePortfolii :: StoreCashTransferF
updatePortfolii conn tfc =
   mapM_ (updateCashReserves conn tfc) . Map.elems
       . snarfL (pure . (port &&& id))
