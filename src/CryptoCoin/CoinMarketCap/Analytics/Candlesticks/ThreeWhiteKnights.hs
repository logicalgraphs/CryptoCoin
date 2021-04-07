{-# LANGUAGE ViewPatterns #-}

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
import Data.CryptoCurrency.Types.Range (between, realBody)
import Data.CryptoCurrency.Types.OCHLV

-- given a set of rows, starting from yesterday and going back in time,
-- let's determine if we have 3 white kngiths

threeWhiteKnights :: [OCHLV] -> Bool
threeWhiteKnights = twk' . take 4

twk' :: [OCHLV] -> Bool
twk' [] = False
twk' [_] = True
twk' (y:t@(db:_)) = y `risingOver` db && twk' t

risingOver :: OCHLV -> OCHLV -> Bool
risingOver (row -> yester) (row -> dayBefore) =
   open yester `between` realBody dayBefore
        && close yester > high dayBefore

-- TODO: shadow must not be overlong ... how do we measure that?

-- test:

sampleBTC :: [OCHLV]
sampleBTC = [
   IxRow 1 (read "2021-03-31") 
         (OCHLVData 58930.277344 59930.027344 57726.417969
                    58918.832031 58918.832031 6.5520826225e10),
   IxRow 1 (read "2021-03-30") 
         (OCHLVData 57750.132813 59447.222656 57251.550781 
                    58917.691406 58917.691406 5.4414116432e10),
   IxRow 1 (read "2021-03-29") 
         (OCHLVData 55947.898438 58342.097656 55139.339844 
                    57750.199219 57750.199219 5.7625587027e10),
   IxRow 1 (read "2021-03-28") 
         (OCHLVData 55874.941406 56610.3125 55071.113281 
                    55950.746094 55950.746094 4.7686580918e10), 
                    -- changed opening to be below tomorrow's opening
   IxRow 1 (read "2021-03-27") 
         (OCHLVData 55137.566406 56568.214844 54242.910156 
                    55973.511719 55973.511719 4.7266542233e10)]

{--
>>> threeWhiteKnights sampleBTC 
True

Data was generated with:

>>> let owrite (OCHLV i d o c h l a v) =
           putStrLn (unwords (["OCHLV", show i, "(read \"" ++ show d ++ "\")"]
                              ++ map show [o, c, h, l, a, v]))
>>> withConnection ECOIN (\conn -> candlesFor conn 1 >>= mapM_ owrite)

--}
