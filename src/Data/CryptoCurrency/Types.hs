module Data.CryptoCurrency.Types where

import Data.Map (Map)

import Data.Time (Day)

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.ToField

import Data.Time.TimeSeries (TimeSeries)
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

instance ToRow r => ToRow (IxRow r) where
   toRow (IxRow i d r) = [toField i, toField d] ++ toRow r
