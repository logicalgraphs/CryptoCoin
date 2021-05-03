{-# LANGUAGE ViewPatterns #-}

module CryptoCoin.CoinMarketCap.Analytics.Candlesticks.AbandonedBaby where

{--
Abandoned Baby

https://www.investopedia.com/articles/active-trading/092315/5-most-powerful-candlestick-patterns.asp#abandoned-baby

The bullish abandoned baby reversal pattern appears at the low of a downtrend, 
after a series of black candles print lower lows. The market gaps lower on the 
next bar, but fresh sellers fail to appear, yielding a narrow range doji 
candlestick with opening and closing prints at the same price. A bullish gap on 
the third bar completes the pattern, which predicts that the recovery will 
continue to even higher highs, perhaps triggering a broader-scale uptrend. 
According to Bulkowski, this pattern predicts higher prices with a 49.73% 
accuracy rate. 
--}

import Data.CryptoCurrency.Types (row, IxRow(IxRow))
import Data.CryptoCurrency.Types.Range (doji)
import Data.CryptoCurrency.Types.OCHLV
import Data.CryptoCurrency.Types.Vector (Vector, vtake, vals)

ab :: Vector OCHLV -> Bool
ab = maybe False (ab' . reverse . vals) . vtake 4

ab' :: [OCHLV] -> Bool
ab' [a,b,c,d] =
      black a
   && b `lowerLow` a
   && black b
   && c `gapsLower` b
   && doji (row c)
   && hollow d
   && d `gapsHigher` c
