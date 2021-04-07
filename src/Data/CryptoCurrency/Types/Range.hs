module Data.CryptoCurrency.Types.Range where

import Data.CryptoCurrency.Types.OCHLV (OCHLVData, open, close, high, low)

data Range = Range { begin, end :: Double }
   deriving (Eq, Ord, Show)

between :: Double -> Range -> Bool
between val (Range b e) = let lo = min b e
                              hi = max b e
                          in  lo <= val && val <= hi

realBody :: OCHLVData -> Range
realBody = Range . open <*> close

shadow :: OCHLVData -> Range
shadow = Range . low <*> high
