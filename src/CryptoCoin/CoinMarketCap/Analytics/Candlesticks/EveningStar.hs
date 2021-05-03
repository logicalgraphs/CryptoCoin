{-# LANGUAGE ViewPatterns #-}

module CryptoCoin.CoinMarketCap.Analytics.Candlesticks.EveningStar where

{--
Evening Star

https://www.investopedia.com/articles/active-trading/092315/5-most-powerful-candlestick-patterns.asp#evening-star

The bearish evening star reversal pattern starts with a tall white bar that 
carries an uptrend to a new high. The market gaps higher on the next bar, but 
fresh buyers fail to appear, yielding a narrow range candlestick. A gap down on 
the third bar completes the pattern, which predicts that the decline will 
continue to even lower lows, perhaps triggering a broader-scale downtrend. 
According to Bulkowski, this pattern predicts lower prices with a 72% accuracy 
rate.
--}

import Data.CryptoCurrency.Types.OCHLV
import Data.CryptoCurrency.Types.Vector (Vector, vtake, vals)

es :: Vector OCHLV -> Bool
es = maybe False (es' . reverse . vals) . vtake 3

es' :: [OCHLV] -> Bool
es' [a,b,c] =
      hollow a         -- how do we measure 'tall,' however?
   && b `gapsHigher` a -- how do we measure 'narrow range'?
   && c `gapsLower` b
   && black c
