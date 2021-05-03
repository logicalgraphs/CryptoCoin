{-# LANGUAGE ViewPatterns #-}

module CryptoCoin.CoinMarketCap.Analytics.Candlesticks.ThreeBlackCrows where

{--
Three Black Crows

https://www.investopedia.com/terms/t/three_black_crows.asp

What Are the Three Black Crows?
Three black crows is a phrase used to describe a bearish candlestick pattern 
that may predict the reversal of an uptrend. Candlestick charts show the day's 
opening, high, low, and closing prices for a particular security. For stocks 
moving higher, the candlestick is white or green. When moving lower, they are 
black or red.

The black crow pattern consists of three consecutive long-bodied candlesticks 
that have opened within the real body of the previous candle and closed lower 
than the previous candle. Often, traders use this indicator in conjunction with 
other technical indicators or chart patterns as confirmation of a reversal.
--}

import Data.CryptoCurrency.Types (row, IxRow(IxRow))
import Data.CryptoCurrency.Types.Range (between, realBody)
import Data.CryptoCurrency.Types.OCHLV
import Data.CryptoCurrency.Types.Vector (Vector, vtake, vals)

tbc :: Vector OCHLV -> Bool
tbc = maybe False (tbc' . vals) . vtake 4

tbc' :: [OCHLV] -> Bool
tbc' [] = False
tbc' [_] = True
tbc' (y:t@(db:_)) = y `fallingDown` db && tbc' t  -- like the London Bridge

fallingDown :: CmpOCHLV
fallingDown (row -> yester) (row -> dayBefore) =
   open yester `between` realBody dayBefore
        && close yester < low dayBefore
