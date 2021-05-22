module Data.CryptoCurrency.Types.Recommendations.Internal where

{--
Because recommendations are across prices, volumes, and candlesticks, AND
across candlestick patterns and trend indicators, we need to do some back-and-
forth marshalling to get to a fully-realized value from the database
--}

import qualified Data.ByteString.Char8 as B
import Data.Time (Day)

import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.Types (Query(Query))

import Data.CryptoCurrency.Types (Idx)
import Data.LookupTable (LookupTable)
import Data.Time.TimeSeries (today)

data RektRow = RR' (Maybe Double) Idx Idx
   deriving (Eq, Ord, Show)

instance ToRow RektRow where
   toRow (RR' d call ind) =
      [toField call, toField ind, toField d]

insertRektQuery :: Query
insertRektQuery = Query . B.pack $ unwords [
   "INSERT INTO recommendation (cmc_id, for_date, call_id, indicator_id,",
   "confidence) VALUES (?, ?, ?, ?, ?)"]

-- Now let's FromRow Recommendation values

data RektReadRow = RRR' (Maybe Double) String String String
   deriving Show

instance FromRow RektReadRow where
   fromRow = RRR' <$> field <*> field <*> field <*> field

fetchAllRektsQueryStrings :: [String]
fetchAllRektsQueryStrings = 
   ["SELECT r.cmc_id, r.for_date, r.confidence, c.call, i.indicator, b.basis",
    "FROM recommendation r",
    "INNER JOIN indicator_lk i ON i.indicator_id=r.indicator_id",
    "INNER JOIN call_lk c ON c.call_id=r.call_id",
    "INNER JOIN basis_lk b ON b.basis_id=i.basis_id"]

fetchAllRektsQuery :: Query
fetchAllRektsQuery = Query . B.pack $ unlines fetchAllRektsQueryStrings

fetchRektsQuery :: Day -> Query
fetchRektsQuery date = Query . B.pack $ unlines (
   fetchAllRektsQueryStrings ++ ["WHERE for_date='" ++ show date ++ "'"])
