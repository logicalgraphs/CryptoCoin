module CryptoCoin.CoinMarketCap.Analytics.Trends.SimpleMovingAverage where

{--
What Is Simple Moving Average (SMA)?
A simple moving average (SMA) calculates the average of a selected range of 
prices, usually closing prices, by the number of periods in that range.

https://www.investopedia.com/terms/s/sma.asp

sma = sum a / length a
--}

import Data.CryptoCurrency.Types (PriceVolume, price, row)
import Data.CryptoCurrency.Types.Vector (Vector, vtake)

-- let's pretend the vector is right-sized before it gets here.

sma :: Vector PriceVolume -> Double
sma v = sum (fmap (price . row) v) / fromIntegral (length v)
