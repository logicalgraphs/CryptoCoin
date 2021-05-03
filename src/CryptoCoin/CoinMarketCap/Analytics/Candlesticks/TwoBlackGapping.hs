module CryptoCoin.CoinMarketCap.Analytics.Candlesticks.TwoBlackGapping where

{--
Two Black Gapping

https://www.investopedia.com/articles/active-trading/092315/5-most-powerful-candlestick-patterns.asp#two-black-gapping

The bearish two black gapping continuation pattern appears after a notable top 
in an uptrend, with a gap down that yields two black bars posting lower lows. 
This pattern predicts that the decline will continue to even lower lows, 
perhaps triggering a broader-scale downtrend. According to Bulkowski, this 
pattern predicts lower prices with a 68% accuracy rate.
--}

import Data.CryptoCurrency.Types.OCHLV
import Data.CryptoCurrency.Types.Vector (Vector, vtake, vals)

tbg :: Vector OCHLV -> Bool
tbg = maybe False (tbg' . reverse . vals) . vtake 5

tbg' :: [OCHLV] -> Bool
tbg' [basis, top, down, gap, lower] =
      top `closesHigher` basis    -- 'uptrend'
   && down `lowerLow` top
   && black down
   && gap `gapsLower` down
   && black gap
   && lower `lowerLow` gap
   && black lower
