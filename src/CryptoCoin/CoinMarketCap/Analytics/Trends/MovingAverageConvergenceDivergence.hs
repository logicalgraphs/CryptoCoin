{-# LANGUAGE TupleSections #-}

module CryptoCoin.CoinMarketCap.Analytics.Trends.MovingAverageConvergenceDivergence where

{--
What Is Moving Average Convergence Divergence (MACD)?
Moving average convergence divergence (MACD) is a trend-following momentum 
indicator that shows the relationship between two moving averages of a 
security’s price. The MACD is calculated by subtracting the 26-period 
exponential moving average (EMA) from the 12-period EMA.

https://www.investopedia.com/terms/m/macd.asp

MACD is calculated by subtracting the long-term EMA (26 periods) from the 
short-term EMA (12 periods). An exponential moving average (EMA) is a type of 
moving average (MA) that places a greater weight and significance on the most 
recent data points.
--}

import Data.Maybe (fromMaybe)

import Data.CryptoCurrency.Types (row)
import Data.CryptoCurrency.Types.PriceVolume (PriceVolume, price)
import Data.CryptoCurrency.Types.Trend
import Data.CryptoCurrency.Types.Vector (Vector)

import CryptoCoin.CoinMarketCap.Analytics.Trends.ExponentialMovingAverage (ema)

ema' :: (TrendData -> Double) -> Trend -> Vector PriceVolume -> Int -> Double
ema' ef t v i = ema ((t, ef (row t)), fromJust $ vtake i v)

macdi :: ((Trend, Maybe Double), Vector PriceVolume) -> Double
macdi (yest, _lastMacd), v) =
   let e9 = ema' ema9 yest v 9
       e12 = ema' ema12 yest v 12
       e26 = ema' ema26 yest v 26
   in  undefined
