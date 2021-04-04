module CryptoCoin.CoinMarketCap.Analytics.Trends.ExponentialMovingAverage where

{--
What Is an Exponential Moving Average (EMA)?
An exponential moving average (EMA) is a type of moving average (MA) that places a greater weight and significance on the most recent data points. The exponential moving average is also referred to as the exponentially weighted moving average. An exponentially weighted moving average reacts more significantly to recent price changes than a simple moving average (SMA), which applies an equal weight to all observations in the period.

https://www.investopedia.com/terms/e/ema.asp

ema today = (value today * smothing / (1 + days))
          + ema yest * (1 - (smoothing / (1 + days)))

where smoothing is usually 2
--}
