{-# LANGUAGE ViewPatterns #-}

module CryptoCoin.CoinMarketCap.ETL.Transfers.Coin where

{-- here we transfer coin between exchanges. The format is:

call,amount,coin,from,to,surcharge,basis
XFER,2021-07-06,0.12445026,COMP,coinbase,gemini,0.0073138,$0.00

you'll have to toUpper the exchanges, FYI.
--}

import Data.Char (toUpper)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Time (Day)

import Database.PostgreSQL.Simple (Connection)

import Control.Scan.CSV (csv,readMaybe)
import CryptoCoin.CoinMarketCap.Data.TrackedCoin (trackedCoins)

import Data.CryptoCurrency.Types (IxRow(IxRow))
import Data.CryptoCurrency.Types.Transfers.Coin
           (storeCoinTransfers, CoinTransfer,
            CoinTransferDatum(CoinTransferDatum))
import Data.CryptoCurrency.Types.Portfolio (portfoliiLk)

import Data.LookupTable (LookupTable)
import Data.Monetary.USD (USD)
import Store.SQL.Connection (withConnection, Database(ECOIN))

sampCoinXfer :: [String]
sampCoinXfer =
   ["XFER,2021-07-06,0.12445026,COMP,coinbase,gemini,0.0073138,$0.00"]

mtu :: String -> String
mtu = map toUpper

toCnXf :: LookupTable -> String -> Maybe CoinTransfer
toCnXf coinLk (tail . csv -> [dt, amt, cn, mtu -> fr, mtu -> xt, sur, bas]) =
   Map.lookup cn coinLk >>= \cid ->
   readMaybe dt         >>= \date ->
   readMaybe amt        >>= \amount ->
   readMaybe sur        >>= \surcharge ->
   readMaybe bas        >>= \basis ->
   return (IxRow cid date (CoinTransferDatum amount surcharge basis fr xt))

{--
>>> import Database.PostgreSQL.Simple
>>> import Store.SQL.Connection 
>>> conn <- connectInfo ECOIN >>= connect
>>> portLk <- portfoliiLk conn
>>> cnLk <- trackedCoins conn
>>> storeCoinTransfers conn portLk (mapMaybe (toCnXf cnLk) sampCoinXfer)

psql> select * from transfer_coin

transfer_coin_id,cmc_id,for_date,amount,surcharge,transfer_from,transfer_to,cost_basis
1,5692,2021-07-06,0.12445026,0.0073138,1,3,0.005
--}

go :: IO ()
go = putStrLn "Not executing coin transfers between exchanges yet."
