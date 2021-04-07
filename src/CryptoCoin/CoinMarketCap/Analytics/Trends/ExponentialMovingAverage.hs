module CryptoCoin.CoinMarketCap.Analytics.Trends.ExponentialMovingAverage where

{--
What Is an Exponential Moving Average (EMA)?
An exponential moving average (EMA) is a type of moving average (MA) that 
places a greater weight and significance on the most recent data points. The 
exponential moving average is also referred to as the exponentially weighted 
moving average. An exponentially weighted moving average reacts more 
significantly to recent price changes than a simple moving average (SMA), 
which applies an equal weight to all observations in the period.

https://www.investopedia.com/terms/e/ema.asp

ema today = (value today * smoothing / (1 + days))
          + ema yest * (1 - (smoothing / (1 + days)))

where smoothing is usually 2

If we don't have yesterday's ema, we use yesterday's sma. EASY-PEASY!
--}

import Data.Maybe (fromMaybe)

import Data.CryptoCurrency.Types (row)
import Data.CryptoCurrency.Types.PriceVolume (PriceVolume, price)
import Data.CryptoCurrency.Types.Trend
import Data.CryptoCurrency.Types.Vector

import CryptoCoin.CoinMarketCap.Analytics.Trends.SimpleMovingAverage (sma)

smoothing :: Double
smoothing = 2

ht :: Vector a -> (a, Vector a)
ht (Vect n (h:t)) = (h, Vect (pred n) t)

ema :: ((Trend, Maybe Double), Vector PriceVolume) -> Double
ema ((_trend, last), v) =
   let (tday, rest) = ht v
       yest = fromMaybe (sma ((mempty, Nothing), rest)) last
       days = fromIntegral (length v)
       smuth = smoothing / days       -- n.b. : days is 1 + days from formula
   in  (price (row tday) * smuth) + (yest * (1 - smuth))
