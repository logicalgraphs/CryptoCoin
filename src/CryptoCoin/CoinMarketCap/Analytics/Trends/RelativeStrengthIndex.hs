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

Then the result is smoothed:

RSIstep2 = 100 -[ 100 / (1 + ((prev avg gain x 13 + current gain)
                      / (prev avg loss x 13) + current loss)) ] 

70% indicates overbuying, 30% indicates overselling

https://www.investopedia.com/terms/r/rsi.asp
--}
