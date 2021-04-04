module Data.CryptoCurrency.Types where

import Data.Map (Map)

import Data.Time (Day)

import Database.PostgreSQL.Simple hiding (close)
import Database.PostgreSQL.Simple.FromRow

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

class Row r where
   row :: r a -> a

type RankVector = Map Idx Integer
type Matrix = TimeSeries RankVector

data IxRow r = IxRow Idx Day r
   deriving (Eq, Ord, Show)

instance Indexed (IxRow a) where
   idx (IxRow i _ _) = i

instance Date (IxRow a) where
   date (IxRow _ d _) = d

instance Row IxRow where
   row (IxRow _ _ r) = r

instance FromRow r => FromRow (IxRow r) where
   fromRow = IxRow <$> field <*> field <*> fromRow

-- CANDLESTICKS -------------------------------------------------------

data OCHLVData = OData { open, close, high, low, adj, volume :: Double }
   deriving (Eq, Ord, Show)

instance FromRow OCHLVData where
   fromRow = OData <$> field <*> field <*> field <*> field <*> field <*> field

type OCHLV = IxRow OCHLVData

data Range = Range { begin, end :: Double }
   deriving (Eq, Ord, Show)

between :: Double -> Range -> Bool
between val (Range b e) = let lo = min b e
                              hi = max b e
                          in  lo <= val && val <= hi

realBody :: OCHLV -> Range
realBody = (Range . open <*> close) . row

shadow :: OCHLV -> Range
shadow = (Range . low <*> high) . row

-- Price/Volume Data (rows) -------------------------------------------

data PVData = PVData { price, vol :: Double }
   deriving (Eq, Ord, Show)

instance FromRow PVData where
   fromRow = PVData <$> field <*> field

type PriceVolume = IxRow PVData
