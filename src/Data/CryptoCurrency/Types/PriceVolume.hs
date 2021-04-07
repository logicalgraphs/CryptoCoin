module Data.CryptoCurrency.Types.PriceVolume where

import qualified Data.ByteString.Char8 as B

import Database.PostgreSQL.Simple (Connection, query)
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.Types (Query(Query))

import Data.CryptoCurrency.Types (IxRow)
import Data.CryptoCurrency.Types.Vector (Vector, mkVect)

import Store.SQL.Connection (withConnection, Database(ECOIN))
import Store.SQL.Util.Indexed

data PVData = PVData { price, vol :: Double }
   deriving (Eq, Ord, Show)

instance FromRow PVData where
   fromRow = PVData <$> field <*> field

type PriceVolume = IxRow PVData

fetchPVQuery :: Query
fetchPVQuery = Query . B.pack $ unwords [
  "SELECT cmc_id, for_date, quote_price, volume_24h",
  "FROM coin_market_cap_daily_listing",
  "WHERE cmc_id=?",
  "ORDER BY for_date DESC",
  "LIMIT 200"]

fetchPricesVolumes :: Connection -> Integer -> IO (Vector PriceVolume)
fetchPricesVolumes conn coinId =
   mkVect <$> query conn fetchPVQuery (Idx coinId)

{--
>>> {
withConnection ECOIN (\conn -> fetchRows conn 1 >>= mapM print >>=
          putStrLn . ("There were " ++) . (++ " rows of data.") . show . length)
}
...
Row {date = 2021-03-10, cmcId = 1, price = 56687.8536484267, vol = 5.70546138680779e10}
Row {date = 2021-03-05, cmcId = 1, price = 47437.3885719626, vol = 4.90061340250151e10}
There were 27 rows of data.
--}
