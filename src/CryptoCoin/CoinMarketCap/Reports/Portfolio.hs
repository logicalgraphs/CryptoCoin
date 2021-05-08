{-# LANGUAGE OverloadedStrings #-}

module CryptoCoin.CoinMarketCap.Reports.Portfolio where

{--
Gives a report of the coins and the balances and the totals of a portfolio.

Woo, boy!
--}

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)

import Database.PostgreSQL.Simple

import CryptoCoin.CoinMarketCap.Types
import CryptoCoin.CoinMarketCap.Types.Quote

import Data.CryptoCurrency.Types
import Data.CryptoCurrency.Types.Portfolio
import Data.CryptoCurrency.Types.Transaction
import Data.Monetary.USD

data Holding = Holding { ecoin                  :: ECoin,
                         amount                 :: Double,
                         invested, price, value :: USD }
   deriving (Eq, Ord, Show)

instance Indexed Holding where
   idx = idx . ecoin

instance Rank Holding where
   rank = rank . ecoin

data PortfolioReport = PR { portfolio                 :: Portfolio,
                            holdings                  :: Set Holding,
                            totalInvested, totalValue :: USD }
   deriving (Eq, Ord, Show)

type Portfolii = Map String PortfolioReport

portfolii :: Connection -> IO Portfolii
portfolii conn = undefined
