module CryptoCoin.CoinMarketCap.Analytics.Trends.RelativeStrengthIndex where

{--
What Is the Relative Strength Index (RSI)?
The relative strength index (RSI) is a momentum indicator used in technical 
analysis that measures the magnitude of recent price changes to evaluate 
overbought or oversold conditions in the price of a stock or other asset. The 
RSI is displayed as an oscillator (a line graph that moves between two 
extremes) and can have a reading from 0 to 100. The indicator was originally 
developed by J. Welles Wilder Jr. and introduced in his seminal 1978 book, 
"New Concepts in Technical Trading Systems."

Traditional interpretation and usage of the RSI are that values of 70 or above 
indicate that a security is becoming overbought or overvalued and may be primed 
for a trend reversal or corrective pullback in price. An RSI reading of 30 or 
below indicates an oversold or undervalued condition.

The standard is to use 14 periods to calculate the initial RSI value. For 
example, imagine the market closed higher seven out of the past 14 days with an 
average gain of 1%. The remaining seven days all closed lower with an average 
loss of -0.8%. The calculation for the first part of the RSI would look like 
the following expanded calculation:

55.55 = 100 - [ 100 / (1 + ((1% / 14) / (abs(-0.8%)/14))) ]

el geophf-comment: note that the 14 in the denominators are redundant.

Then the result is smoothed:

RSIstep2 = 100 - [ 100 / (1 + ((prev avg gain x 13 + current gain)
                      / (prev avg loss x 13) + current loss)) ] 

70% indicates overbuying, 30% indicates overselling

https://www.investopedia.com/terms/r/rsi.asp

el geophf-comment: how is step 1 incorporated into step 2, because I don't
see it. What are the calculations of 'prev avg loss'? What is 'current loss'?

Do I just throw RSIstep2 on the ground?
--}

import Data.List (partition)

import Data.Maybe (fromJust)

import Data.CryptoCurrency.Types (row)
import Data.CryptoCurrency.Types.PriceVolume (PriceVolume, price)
import Data.CryptoCurrency.Types.Vector (Vector, vals, mkVect, vtake)

rsi :: ((trend, Maybe double), Vector PriceVolume) -> Double
rsi ((_t, _mbrsi), v) =
   let (gains, losses) = computer v
   in  100 - (100 / succ (gains / abs losses))

{--
This might be step 2, but, then again, anything might be step two, for heaven's
sake!

   let (gurr, lurr) = computer v
       (grev, lrev) = computer (fromJust (vtake (pred (length v)) v))
   in  100 - (100 / (succ (grev * 13 + gurr)) / (lrev * 13 + lurr))
--}
       
computer :: Vector PriceVolume -> (Double, Double)
computer v =
   let prices = map (price . row) (vals v)
       diffs  = zipWith (-) prices (tail prices)
       percs  = zipWith (/) diffs prices
       bitrav f (as, bs) = (f as, f bs)
       gainsNlosses = bitrav mkVect (partition (> 0) percs)
       avg x = sum x / fromIntegral (length x)
   in  bitrav avg gainsNlosses
