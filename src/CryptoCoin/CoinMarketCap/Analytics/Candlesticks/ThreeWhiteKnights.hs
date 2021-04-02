module CryptoCoin.CoinMarketCap.Analytics.Candlesticks.ThreeWhiteKnights where

{--
Three White Knights, called Three White Soldiers by investopedia

https://www.investopedia.com/terms/t/three_white_soldiers.asp

Given the last three days of candlesticks (period: 1d), 

What Do Three White Soldiers Mean?
Three white soldiers is a bullish candlestick pattern that is used to predict 
the reversal of the current downtrend in a pricing chart. The pattern consists 
of three consecutive long-bodied candlesticks that open within the previous 
candle's real body and a close that exceeds the previous candle's high. These 
candlesticks should not have very long shadows and ideally open within the 
real body of the preceding candle in the pattern.

real body:

https://www.investopedia.com/terms/r/realbody.asp

What is the Real Body?
The real body is the wide part of a candle on a candlestick chart. The real 
body covers the area between the opening price and the closing price for a 
period of time. If the open is below the close the candle is often colored 
green or white. If the close is below the open, for the time period, the 
candle is usually colored red or black.

shadow:

https://www.investopedia.com/terms/s/shadow.asp

What Is a Shadow?
A shadow, or a wick, is a line found on a candle in a candlestick chart that is 
used to indicate where the price of a stock has fluctuated relative to the 
opening and closing prices. Essentially, these shadows illustrate the highest 
and lowest prices at which a security has traded over a specific time period. 
The candlestick also has a wide part, which is called the "real body."
--}

import Data.CryptoCurrency.Types  -- for the candlesticks/OCHLV

-- given a set of rows, starting from yesterday and going back in time,
-- let's determine if we have 3 white kngiths

threeWhiteKnights :: [OCHLV] -> Bool
threeWhiteKnights = twk' . take 4

twk' :: [OCHLV] -> Bool
twk' [_] = True
twk' (y:t@(db:_)) = y `risingOver` db && twk' t

risingOver :: OCHLV -> OCHLV -> Bool
risingOver yester dayBefore = open yester `between` realBody dayBefore
                           && close yester > high dayBefore

-- TODO: shadow must not be overlong ... how do we measure that?
