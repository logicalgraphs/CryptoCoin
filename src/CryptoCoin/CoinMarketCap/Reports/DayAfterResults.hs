{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module CryptoCoin.CoinMarketCap.Reports.DayAfterResults where

import Control.Arrow ((&&&))

import qualified Data.ByteString.Char8 as B
import Data.List (sortOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Time (Day, addDays)

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.Types (Query(Query))

import Control.Presentation (Univ, explode)

import CryptoCoin.CoinMarketCap.Reports.Table (csvReport)
import CryptoCoin.CoinMarketCap.Utils (geaux)

import Data.CryptoCurrency.Types (Idx, Rank, rank, row, idx)
import Data.CryptoCurrency.Types.Recommendation
    (Call(BUY, SELL), call, fetchRecommendations, Recs, fetchRecs)

import Data.Percentage
import Data.Monetary.USD

-- shows the results of yesterday's recommendations

fetchYestRecs :: Connection -> Day -> IO Recs
fetchYestRecs conn = fetchRecs conn . addDays (-1)

allResultsQuery :: Query
allResultsQuery = Query . B.pack $ unlines [
   "select b.cmc_id, c.symbol, b.for_date, a.for_date,",
                              "b.rank, a.rank,",
                              "b.quote_price,a.quote_price",
   "from coin_market_cap_daily_listing a",
   "inner join coin_market_cap_daily_listing b on a.cmc_id=b.cmc_id",
   "inner join coin c on c.cmc_id=b.cmc_id",
   "where a.for_date = ? and b.for_date= ? and b.cmc_id in ?",
   "order by b.rank"]

data Result = Result { cmcId :: Idx, sym    :: String,
                       yest, tday           :: Day,
                       yestRank, tdayRank   :: Integer,
                       yestPrice, tdayPrice :: USD }
   deriving (Eq, Ord, Show)

data ResultInput = RI Day [Idx]

instance ToRow ResultInput where
   toRow (RI t ids) = map toField [t, addDays (-1) t] ++ [toField (In ids)]

instance Rank Result where
   rank = yestRank

instance Univ Result where
   explode r =
      let (USD yp) = yestPrice r
          (USD tp) = tdayPrice r
          yr = yestRank r
          tr = tdayRank r in
      [show (cmcId r), sym r,
       show (tday r), show tr, show (USD tp),
       show (yest r), show yr, show (USD yp),
       show (tr - yr), show (P ((tp - yp) / tp))]

instance FromRow Result where
   fromRow = Result <$> field <*> field <*> field <*> field
                                        <*> field <*> field
                                        <*> field <*> field

allResults :: Connection -> Day -> Recs -> IO [Result]
allResults conn tday = query conn allResultsQuery . RI tday . Map.keys

data RecResult = RR Result Call

instance Rank RecResult where rank (RR r c) = rank r
instance Univ RecResult where explode (RR r c) = explode r ++ [show c]

mkRR :: Recs -> Result -> Maybe RecResult
mkRR rs r = RR r <$> Map.lookup (cmcId r) rs

allResultsAndReport :: Connection -> Day -> IO ()
allResultsAndReport conn date =
   fetchYestRecs conn date   >>= \recs ->
   allResults conn date recs >>=
   csvReport date "result" rhdrs . mapMaybe (mkRR recs)

rhdrs :: [String]
rhdrs = words ("id sym today rank price yesterday rank "
            ++ "price rank-diff delta-price call")

go :: IO ()
go = geaux allResultsAndReport
