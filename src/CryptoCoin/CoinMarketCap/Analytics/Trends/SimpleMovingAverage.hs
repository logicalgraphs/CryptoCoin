module CryptoCoin.CoinMarketCap.Analytics.Trends.SimpleMovingAverage where

{--
What Is Simple Moving Average (SMA)?
A simple moving average (SMA) calculates the average of a selected range of 
prices, usually closing prices, by the number of periods in that range.

https://www.investopedia.com/terms/s/sma.asp

sma = sum a / length a
--}

import Data.CryptoCurrency.Types (row)
import Data.CryptoCurrency.Types.PriceVolume (PriceVolume, price)
import Data.CryptoCurrency.Types.Trend
import Data.CryptoCurrency.Types.Vector (Vector)

-- let's pretend the vector is right-sized before it gets here.

sma :: ((Trend, Maybe Double), Vector PriceVolume) -> Double
sma (_trendctx, v) = sum (fmap (price . row) v) / fromIntegral (length v)
