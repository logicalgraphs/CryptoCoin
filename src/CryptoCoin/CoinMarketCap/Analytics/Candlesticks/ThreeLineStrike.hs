{-# LANGUAGE ViewPatterns #-}

module CryptoCoin.CoinMarketCap.Analytics.Candlesticks.ThreeLineStrike where

{--
Three Line Strike

https://www.investopedia.com/articles/active-trading/092315/5-most-powerful-candlestick-patterns.asp#three-line-strike

The bullish three line strike reversal pattern carves out three black candles 
within a downtrend. Each bar posts a lower low and closes near the intrabar 
low. The fourth bar opens even lower but reverses in a wide-range outside bar 
that closes above the high of the first candle in the series. The opening print 
also marks the low of the fourth bar. According to Bulkowski, this reversal 
predicts higher prices with an 83% accuracy rate.
--}

import Data.CryptoCurrency.Types (row)
import Data.CryptoCurrency.Types.OCHLV
import Data.CryptoCurrency.Types.Vector

tls :: Vector OCHLV -> Bool
tls = maybe False (tls' . vals) . vtake 4

tls' :: [OCHLV] -> Bool
tls' ts@[y, _, _, f] = y `closesHigher` f && tls'' ts

tls'' :: [OCHLV] -> Bool
tls'' [] = False
tls'' [_] = True
tls'' (y:t@(db:_)) = y `lowerLow` db && tls'' t
