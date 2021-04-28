{-# LANGUAGE ViewPatterns #-}

module Data.CryptoCurrency.Types.OCHLV where

import qualified Data.ByteString.Char8 as B

import Database.PostgreSQL.Simple hiding (close)
import Database.PostgreSQL.Simple.Types (Query(Query))
import Database.PostgreSQL.Simple.FromRow

import Data.CryptoCurrency.Types (IxRow, Idx, row)
import Data.CryptoCurrency.Types.Vector (Vector, mkVect)

data OCHLVData = OCHLVData { open, close, high, low, adj, volume :: Double }
   deriving (Eq, Ord, Show)

instance FromRow OCHLVData where
   fromRow = OCHLVData <$> field <*> field <*> field
                       <*> field <*> field <*> field

-- the FromRow is for this query:

candlesQuery :: Query
candlesQuery = Query . B.pack $ unwords [
   "SELECT cmc_id, for_date, open, close, high, low, adjusted_close, volume",
   "FROM candlesticks WHERE cmc_id=? ORDER BY for_date DESC LIMIT ?"]

candlesFor :: Connection -> Idx -> IO (Vector OCHLV)
candlesFor conn cmcId = mkVect <$> query conn candlesQuery (cmcId, 5 :: Integer)

-- so for some tracked coin, we load in the OCHLV for the last x days
-- and return a set of patterns signaled along with their confidence measures

type OCHLV = IxRow OCHLVData

type CmpOCHLV = OCHLV -> OCHLV -> Bool

lowerLow :: CmpOCHLV 
lowerLow (row -> tday) (row -> yest) =
   low tday < low yest

closesHigher :: CmpOCHLV
closesHigher (row -> tday) (row -> wayBack) =
   close tday > high wayBack
