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

import Data.Maybe (mapMaybe, maybeToList)

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow

import CryptoCoin.CoinMarketCap.Analytics.Trends.SimpleMovingAverage (sma)
import CryptoCoin.CoinMarketCap.Analytics.Trends.ExponentialMovingAverage (ema)
import CryptoCoin.CoinMarketCap.Analytics.Trends.MovingAverageConvergenceDivergence (macdi)
import CryptoCoin.CoinMarketCap.Analytics.Trends.RelativeStrengthIndex (rsi)
import CryptoCoin.CoinMarketCap.Analytics.Trends.OnBalanceVolume (obvi)

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
type IndicatorBase = (TrendData -> Maybe Double, PVctx -> Double)
type IndicatorA = (Int -> Int, IndicatorBase)
type Indicators = Map (Indicator, Int) IndicatorA

data Temporal = Temporal (Map (Indicator, Int) IndicatorA)
data Eternal = Eternal (Map Indicator IndicatorA)

class LookInto a where
   lookin :: a -> (Indicator, Int) -> Maybe IndicatorA

instance LookInto Temporal where
   lookin (Temporal m) = flip Map.lookup m

instance LookInto Eternal where
   lookin (Eternal m) = flip Map.lookup m . fst

temporal :: Temporal
temporal = Temporal $ Map.fromList [
   ((SimpleMovingAverage, 50), (id, (sma50, sma))),
   ((SimpleMovingAverage, 200), (id, (sma200, sma))),
   ((ExponentialMovingAverage, 9), (succ, (ema9, ema))),
   ((ExponentialMovingAverage, 12), (succ, (ema12, ema))),
   ((ExponentialMovingAverage, 26), (succ, (ema26, ema)))]

eternal :: Eternal
eternal = Eternal $ Map.fromList [
   (MovingAverageConvergenceDivergence, (const 27, (macd, macdi))),
   (RelativeStrengthIndex, (const 15, (rsi14, rsi))),
   (OnBalanceVolume, (const 10, (obv, obvi)))]   -- I DON'T KNOW! :/

data Dependency = T Temporal | E Eternal

instance LookInto Dependency where
   lookin (T t) = lookin t
   lookin (E e) = lookin e

guardedIndicator :: PVdom -> Int -> IndicatorA -> Maybe Double
guardedIndicator (t, v) sz (nf, (tf, f)) =
   f <$> sequence ((id &&& tf . row) t, vtake (nf sz) v)

btc :: Indicator -> Int -> IO ()
btc = btc' [T temporal, E eternal]

btc' :: LookInto indicators => [indicators] -> Indicator -> Int -> IO ()
btc' indies ind i =
   let mbIndA = mapMaybe (flip lookin (ind, i)) indies
       mbG v = mbIndA >>= maybeToList . guardedIndicator v i
   in  withConnection ECOIN (\conn ->
          fetchLastTrend conn 1     >>= \trend ->
          fetchPricesVolumes conn 1 >>= \pv ->
          print (mbG (trend, pv)))

{--
>>> btc ExponentialMovingAverage 12
[59134.597811008054]

>>> btc SimpleMovingAverage 200
[]

and for the ones where the dates are preset:

>>> btc MovingAverageConvergenceDivergence 26
[1411.982019920164]

>>> btc RelativeStrengthIndex 234234  -- because days don't matter
[48.53384918758772]

Bitcoin is just chugging along, isn't it.

>>> btc OnBalanceVolume 10
[-8.32027697067179e10]

But may be oversold?
--}
