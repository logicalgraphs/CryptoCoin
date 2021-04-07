{-# LANGUAGE OverloadedStrings #-}

module CryptoCoin.CoinMarketCap.Analytics.Trends.Indicators where

{--
Like the Candlestick patterns, these are the "Trend Trading: The 4 Common
Indicators" outside the candlesticks (focusing more on price, ... sometimes).

--}

{--
Need:

cmc_id, volume_24h, for_date, quote_price

Need: 50 an 200 for sma, 12 and 26 for EMA / MACD, 15 for RSI, 
OBV just needs the previous day and today.
--}

import Control.Arrow ((&&&))

import Data.Map (Map)
import qualified Data.Map as Map

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow

import CryptoCoin.CoinMarketCap.Analytics.Trends.SimpleMovingAverage (sma)
import CryptoCoin.CoinMarketCap.Analytics.Trends.ExponentialMovingAverage (ema)

import Data.CryptoCurrency.Types (row)
import Data.CryptoCurrency.Types.PriceVolume
import Data.CryptoCurrency.Types.Trend
import Data.CryptoCurrency.Types.Vector (Vector, vtake)

import Store.SQL.Connection (withConnection, Database(ECOIN))

data Indicator = SimpleMovingAverage
               | ExponentialMovingAverage
               | MovingAverageConvergenceDivergence
               | RelativeStrengthIndex
               | OnBalanceVolume
   deriving (Eq, Ord, Show)

type PVdom = (Trend, Vector PriceVolume)
type PVctx = ((Trend, Maybe Double), Vector PriceVolume)
type IndicatorA = (Int -> Int, TrendData -> Maybe Double, PVctx -> Double)
type Indicators = Map (Indicator, Int) IndicatorA

indicators :: Indicators
indicators = Map.fromList [
   ((SimpleMovingAverage, 50), (id, sma50, sma)),
   ((SimpleMovingAverage, 200), (id, sma200, sma)),
   ((ExponentialMovingAverage, 9), (succ, ema9, ema)),
   ((ExponentialMovingAverage, 12), (succ, ema12, ema)),
   ((ExponentialMovingAverage, 26), (succ, ema26, ema)),
   ((MovingAverageConvergenceDivergence, 26), (succ, macd, macdi))
   ]

guardedIndicator :: PVdom -> Int -> IndicatorA -> Maybe Double
guardedIndicator (t, v) sz (nf, tf, f) =
   f <$> sequence ((id &&& tf . row) t, vtake (nf sz) v)

btc :: Indicator -> Int -> IO ()
btc ind i =
   let mbIndA = Map.lookup (ind, i) indicators
       mbG v = mbIndA >>= guardedIndicator v i
   in  withConnection ECOIN (\conn ->
          fetchLastTrend conn 1     >>= \trend ->
          fetchPricesVolumes conn 1 >>= \pv ->
          print (mbG (trend, pv)))

{--
>>> btc ExponentialMovingAverage 12
Just 57988.46654227555

>>> btc SimpleMovingAverage 200
Nothing
--}
