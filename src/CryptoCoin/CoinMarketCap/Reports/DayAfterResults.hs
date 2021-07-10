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
    (Call(BUY, SELL), call, fetchRecommendations)

import Data.Percentage
import Data.Monetary.USD

-- shows the results of yesterday's recommendations

{--
World's crazyest SQL:

WITH recs(cmcId, buys, sells) AS (select * from crosstab($$
select r.cmc_id as cmcId, cl.call as cll, count(cl.call)
from recommendation r
inner join call_lk cl on cl.call_id=r.call_id
where r.for_date='2021-07-07'
group by cmcId, cll
					   $$,
					   $$VALUES ('BUY'::text), ('SELL')$$)
as ct ("cmcId" bigint, "buys" bigint, "sells" bigint))
select r.cmcId, c.symbol, b.rank as yestRank, a.rank as tdayRank, 
b.quote_price as yestPrice,a.quote_price as tdayPrice, r.buys, r.sells
from recs r
inner join coin_market_cap_daily_listing b on r.cmcId=b.cmc_id
inner join coin_market_cap_daily_listing a on r.cmcId=a.cmc_id
inner join coin c on c.cmc_id=b.cmc_id
where a.for_date ='2021-07-08' and b.for_date='2021-07-07'
order by b.rank

... but we're not going to do this, I just showed you that you could, is all.

Okay, back to reality. First we fetch the recommendation from yesterday,
and return a mapping of idxn to buy-sell counts.

I'm going off the assumption that ANY sell indicator means sell. That's how
it's kinda played out in the markets.
--}

type Recs = Map Idx Call

fetchYestRecs :: Connection -> Day -> IO Recs
fetchYestRecs conn (addDays (-1) -> yest) =
   Map.fromList . sortOn snd . map (idx &&& call . row)
   <$> fetchRecommendations conn yest

allRecsQuery :: Query
allRecsQuery = Query . B.pack $ unlines [
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
allResults conn tday = query conn allRecsQuery . RI tday . Map.keys

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
