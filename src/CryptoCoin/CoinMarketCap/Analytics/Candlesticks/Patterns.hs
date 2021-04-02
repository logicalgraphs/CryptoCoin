module CryptoCoin.CoinMarketCap.Analytics.Candlesticks.Patterns where

{--
We take our tracked securities' candlesticks and run them against patterns
to generate buy/sell/hold recommendations.
--}

import Data.Map (Map)
import qualified Data.Map as Map

import CryptoCoin.CoinMarketCap.Analytics.Candlesticks.ThreeWhiteKnights (threeWhiteKnights)

import Data.CryptoCurrency.Types

import Data.Percentage

data Pattern = ThreeWhiteKnights
             | ThreeLineStrikes
             | ThreeBlackCrows
             | AbandonedBaby
             | TwoBlackGapping
             | EveningStar
   deriving (Eq, Ord, Show)

type Signal = [OCHLV] -> Bool

undef :: Signal      -- the 'not yet' signal
undef = const False

confidences :: Map Pattern (Signal, Percent)
confidences = Map.fromList [
   (ThreeLineStrike,   (undef, P 83))
   (TwoBlackGapping,   (undef, P 68)),
   (ThreeBlackCrows,   (undef, P 78)),
   (EveningStar,       (undef, P 72)),
   (AbandonedBaby,     (undef, P 50)),
   (ThreeWhiteKnights, (threeWhiteKnights, P 91))]
