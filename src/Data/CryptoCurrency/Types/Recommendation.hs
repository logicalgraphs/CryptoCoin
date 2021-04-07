module Data.CryptoCurrency.Types.Recommendation where

import Data.Percentage

data Call = BUY | SELL
   deriving (Eq, Ord, Show)

data Recommendation = Rec Call Percentage
   deriving (Eq, Ord, Show)
