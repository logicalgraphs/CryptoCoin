module Data.CryptoCurrency.Types.Conversions.Internal where

-- converts between conversions and their database forms

import qualified Data.ByteString.Char8 as B

import Database.PostgreSQL.Simple (Connection)
import Database.PostgreSQL.Simple.ToField (toField)
import Database.PostgreSQL.Simple.ToRow (ToRow, toRow)
import Database.PostgreSQL.Simple.Types (Query(Query))

import Data.CryptoCurrency.Types (Idx, IxRow(IxRow))

import Data.Monetary.USD (USD)

import Data.XHTML (Name)

data Spot' = Spot' Idx Double USD
   deriving (Eq, Ord, Show)

instance ToRow Spot' where
   toRow (Spot' ix amt quot) = [toField ix, toField amt, toField quot]

data ConvertData' =
   CD' Spot' Spot' Double Double Double (Maybe Name)
      deriving (Eq, Ord, Show)

insertConversionQuery :: Query
insertConversionQuery = Query . B.pack $ unlines [
   "INSERT INTO convert_coin",
   "(portfolio_id, for_date,",
   " from_cmc_id, from_amount, from_quote_price,",
   " to_cmc_id, to_amount, to_quote_price,",
   " coin_fee, commission, tax, confirmation)",
   "VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"]

instance ToRow ConvertData' where
   toRow (CD' coin0 coin1 fee commission tax confirmation) =
       concat (map toRow [coin0, coin1])
       ++ map toField [fee,commission,tax]
       ++ [toField confirmation]

type Convert' = IxRow ConvertData'
