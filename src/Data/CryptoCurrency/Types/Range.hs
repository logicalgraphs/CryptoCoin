module Data.CryptoCurrency.Types.Range where

-- we do a lot of this stuff in candlestick-analyses, so, the range-type
-- gives us a more direct way of talking about these patterns.

import Data.CryptoCurrency.Types.OCHLV (OCHLVData, open, close, high, low)

data Range = Range { begin, end :: Double }
   deriving (Eq, Ord, Show)

between :: Double -> Range -> Bool
between val (Range b e) = let lo = min b e
                              hi = max b e
                          in  lo <= val && val <= hi

magnitude :: Range -> Double
magnitude = (-) . rmax <*> rmin

rmax :: Range -> Double
rmax = max . begin <*> end

rmin :: Range -> Double
rmin = min . begin <*> end

delta :: Range -> Double
delta = (/) . magnitude <*> rmin

doji :: OCHLVData -> Bool
doji = (< 0.05) . delta . realBody

realBody :: OCHLVData -> Range
realBody = Range . open <*> close

shadow :: OCHLVData -> Range
shadow = Range . low <*> high
