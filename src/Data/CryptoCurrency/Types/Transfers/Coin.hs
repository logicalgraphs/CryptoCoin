{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module Data.CryptoCurrency.Types.Transfers.Coin where

{--
Transfer coins from portfolio to portfolio.

The table lay-out:

(transfer_coin_id)
for_date
cmc_id
amount (of coin transferred)
surcharge (as coin)
transfer_from: Portfolio (id)
transfer_to: ditto
cost_bases (like: $100)
--}

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)

import Database.PostgreSQL.Simple (Connection)

import Data.CryptoCurrency.Types (IxRow(IxRow), Idx)
import Data.CryptoCurrency.Types.Transfers.Internal
           (CoinTransF'(Cnxf), fetchAllCoinTransFs, storeCnXs)
import Data.LookupTable (LookDown, lookdown, LookupTable)
import Data.Monetary.USD (USD)
import Data.XHTML (Name)

data CoinTransferDatum =
   CoinTransferDatum { amount, surcharge :: Double, 
                       basis             :: USD,
                       xfrom, xto        :: Name }
      deriving (Eq, Ord, Show)

type CoinTransfer = IxRow CoinTransferDatum

fetchCoinTransfers :: Connection -> LookupTable -> IO [CoinTransfer]
fetchCoinTransfers conn (lookdown -> portLd) =
   mapMaybe (fromCnX' portLd) <$> fetchAllCoinTransFs conn

fromCnX' :: LookDown String -> CoinTransF' -> Maybe CoinTransfer
fromCnX' portLd (Cnxf d amt cid chrg bas f t) =
   IxRow cid d <$> (CoinTransferDatum amt chrg bas <$> lk f <*> lk t)
      where lk = flip Map.lookup portLd

toCnX' :: LookupTable -> CoinTransfer -> Maybe CoinTransF'
toCnX' portLk (IxRow ix dt (CoinTransferDatum amt sur bas fr xto)) =
   Cnxf dt amt ix sur bas <$> lk fr <*> lk xto
      where lk = flip Map.lookup portLk

storeCoinTransfers :: Connection -> LookupTable -> [CoinTransfer] -> IO ()
storeCoinTransfers conn portLk = storeCnXs conn . mapMaybe (toCnX' portLk)
