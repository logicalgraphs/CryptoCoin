module Data.CryptoCurrency.Types where

import Data.Map (Map)

import Data.Time (Day)

import Data.Time.TimeSeries
import Data.XHTML (Name)

type Idx = Integer

class Indexed a where
   idx :: a -> Idx

class Named a where
   namei :: a -> Name

type Symbol = String

class Cymbal a where   -- lol
   sym :: a -> Symbol

class Rank a where
   rank :: a -> Integer

class Date a where
   date :: a -> Day

type RankVector = Map Idx Integer
type Matrix = TimeSeries RankVector

-- CANDLESTICKS -------------------------------------------------------

data OCHLV = OCHLV { coinId :: Idx, forDay :: Day,
                     open, close, high, low, adj, volume :: Double }
   deriving (Eq, Ord, Show)

data Range = Range { begin, end :: Double }
   deriving (Eq, Ord, Show)

between :: Double -> Range -> Bool
between val (Range b e) = let lo = min b e
                              hi = max b e
                          in  lo <= val && val <= hi

realBody :: OCHLV -> Range
realBody = Range . open <*> close

shadow :: OCHLV -> Range
shadow = Range . low <*> high
