{-# LANGUAGE ViewPatterns #-}

module Data.CryptoCurrency.Types.Recommendation where

-- Houses recommendations

import Data.Char (isLower)
import Data.List (groupBy)
import qualified Data.Map as Map
import Data.Time (Day)

import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.ToField

import Data.CryptoCurrency.Types
import Data.LookupTable (LookupTable)
import Data.Percentage

data Call = BUY | SELL
   deriving (Eq, Ord, Show)

data RecommendationData = RekT Call Indicator Percentage
                        | RekP Call Pattern Percentage
   deriving (Eq, Ord, Show)

data Indicator = SimpleMovingAverage
               | ExponentialMovingAverage
               | MovingAverageConvergenceDivergence
               | RelativeStrengthIndex
               | OnBalanceVolume
   deriving (Eq, Ord, Show)

data Pattern = ThreeWhiteKnights
             | ThreeLineStrike
             | ThreeBlackCrows
             | AbandonedBaby
             | TwoBlackGapping
             | EveningStar
   deriving (Eq, Ord, Show)

type Recommendation = IxRow RecommendationData

data RektRow = RR' Double Idx Idx
   deriving (Eq, Ord, Show)

toRektRow :: RecommendationData -> LookupTable -> LookupTable -> Maybe RektRow
toRektRow (RekT c i (P p)) = trr' c i p
toRektRow (RekP c pat (P p)) = trr' c pat p

trr' :: Show a => Call -> a -> Rational -> LookupTable -> LookupTable
     -> Maybe RektRow
trr' (show -> c) (deCamelCase -> str) (fromRational . (/ 100) -> p) cLk iLk =
   RR' p <$> Map.lookup c cLk <*> Map.lookup str iLk

{--
>>> withConnection ECOIN (\conn -> lookupTable conn "call_lk" >>= \cLk -> 
          lookupTableFrom conn "select indicator_id,indicator FROM indicator_lk"
                                 >>= \iLk -> 
          print $ toRektRow (RekT BUY SimpleMovingAverage (P 44)) cLk iLk)
Just (RR' 0.44 1 7)

>>> withConnection ECOIN (\conn -> lookupTable conn "call_lk" >>= \cLk -> 
          lookupTableFrom conn "select indicator_id,indicator FROM indicator_lk"
                                 >>= \iLk -> 
          print $ toRektRow (RekP SELL AbandonedBaby (P 32)) cLk iLk)
Just (RR' 0.32 2 4)

... converting the other way will be ... 'fun.'
--}

instance ToRow RektRow where
   toRow (RR' d call ind) =
      [toField call, toField ind, toField d]

deCamelCase :: Show a => a -> String
deCamelCase = unwords . groupBy (curry $ isLower . snd) . show
   -- via p h z @phaazon_

{--
>>> deCamelCase SimpleMovingAverage 
"Simple Moving Average"
--}
