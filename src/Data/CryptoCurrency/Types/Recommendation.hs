{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module Data.CryptoCurrency.Types.Recommendation where

-- Houses recommendations

import Control.Arrow ((&&&))
import Control.Monad (join)

import qualified Data.ByteString.Char8 as B

import Data.Char (isLower)
import Data.List (groupBy, sortOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Time (Day)

import Database.PostgreSQL.Simple (Connection, executeMany, query_)
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.Types (Query(Query))

import Control.Map (snarf)
import Control.Scan.CSV (readMaybe)

import Data.CryptoCurrency.Types
import Data.CryptoCurrency.Types.Recommendations.Internal
import Data.CryptoCurrency.Utils (report, plural)

import Data.LookupTable (LookupTable)
import Data.Percentage
import Data.Time.TimeSeries (today)

import Store.SQL.Connection (withConnection, Database(ECOIN))
import Store.SQL.Util.LookupTable (lookupTableFrom)

data Call = BUY | SELL
   deriving (Eq, Ord, Show, Read)

dir :: Call -> Double
dir BUY = 1
dir SELL = (-1)

instance Monoid Call where
   mempty = BUY
   BUY `mappend` BUY = BUY
   _   `mappend` _   = SELL

data Source = Pat Pattern | Ind Indicator
   deriving (Eq, Ord, Show)

data RecommendationData =
   Rekt { call :: Call, source :: Source, confidence :: Maybe Percentage }
      deriving (Eq, Ord, Show)

instance Monoid RecommendationData where
   mempty = undefined
   a `mappend` b = b

data Indicator = SimpleMovingAverage
               | ExponentialMovingAverage
               | MovingAverageConvergenceDivergence
               | RelativeStrengthIndex
               | OnBalanceVolume
   deriving (Eq, Ord, Show, Read)

data Pattern = ThreeWhiteKnights
             | ThreeLineStrike
             | ThreeBlackCrows
             | AbandonedBaby
             | TwoBlackGapping
             | EveningStar
   deriving (Eq, Ord, Show, Read)

data Basis = CANDLESTICK | PRICE | VOLUME
   deriving (Eq, Ord, Show, Read)

type Recommendation = IxRow RecommendationData

toIxRektRow :: LookupTable -> LookupTable -> Recommendation
            -> Maybe (IxRow RektRow)
toIxRektRow cLk iLk (IxRow ix date rekt) =
   IxRow ix date <$> toRektRow rekt cLk iLk

toRektRow :: RecommendationData -> LookupTable -> LookupTable -> Maybe RektRow
toRektRow (Rekt c i p) = trr' c i (fromRational . percent <$> p)

trr' :: Call -> Source -> Maybe Double -> LookupTable -> LookupTable
     -> Maybe RektRow
trr' (show -> c) (dCamelCase' -> str) p cLk iLk =
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

-- inserting a recommendation

lookupInds :: Connection -> IO LookupTable
lookupInds =
   flip lookupTableFrom "SELECT indicator_id, indicator FROM indicator_lk"

insertRecommendations :: Connection -> LookupTable -> LookupTable
                      -> [Recommendation] -> IO ()
insertRecommendations conn callLk indLk rex =
   let sz = length rex in
   report 0
          (unwords ["Inserting", show sz, "recommendation" ++ plural sz ++ "."])
          (let recs2insert = mapMaybe (toIxRektRow callLk indLk) rex in
           executeMany conn insertRektQuery recs2insert)

dCamelCase' :: Source -> String
dCamelCase' (Ind i) = deCamelCase i
dCamelCase' (Pat p) = deCamelCase p

deCamelCase :: Show a => a -> String
deCamelCase = unwords . groupBy (curry $ isLower . snd) . show
   -- via p h z @phaazon_

{--
>>> deCamelCase SimpleMovingAverage 
"Simple Moving Average"
--}

-- Fetching Recommendation values from the database

fetchRecommendations :: Connection -> Day -> IO [Recommendation]
fetchRecommendations conn date =
   mapMaybe toRecsRekts <$> query_ conn (fetchRektsQuery date)

toRecsRekts :: IxRow RektReadRow -> Maybe Recommendation
toRecsRekts (IxRow i d (RRR' mbprice cal ind bas)) =
   IxRow i d <$> (Rekt <$> mbr cal <*> toSource ind bas
             <*> Just (P . toRational <$> mbprice))
      -- "Just maybe price" ... lol
      where mbr = readMaybe

toSource :: String -> String -> Maybe Source
toSource ind bas = join $ readSource ind <$> readMaybe bas

readSource :: String -> Basis -> Maybe Source
readSource ind CANDLESTICK = Pat <$> readMaybe (smoosh ind)
readSource ind _           = Ind <$> readMaybe (smoosh ind)

{--
>>> today >>= \tday -> withConnection ECOIN (\conn -> 
            fetchRecommendations conn (addDays (-1) tday) >>= mapM_ print)
IxRow 3783 2021-04-20 (Rekt BUY (Pat ThreeWhiteKnights) (Just 91.00%))
IxRow 1856 2021-04-20 (Rekt BUY (Pat ThreeWhiteKnights) (Just 91.00%))
IxRow 2099 2021-04-20 (Rekt BUY (Pat ThreeWhiteKnights) (Just 91.00%))
IxRow 1966 2021-04-20 (Rekt BUY (Pat ThreeWhiteKnights) (Just 91.00%))
IxRow 1567 2021-04-20 (Rekt BUY (Pat ThreeWhiteKnights) (Just 91.00%))
IxRow 1376 2021-04-20 (Rekt BUY (Pat ThreeWhiteKnights) (Just 91.00%))
IxRow 1772 2021-04-20 (Rekt BUY (Pat ThreeWhiteKnights) (Just 91.00%))
IxRow 1437 2021-04-20 (Rekt BUY (Pat ThreeWhiteKnights) (Just 91.00%))
--}

smoosh :: String -> String
smoosh = concat . words

{--
>>> smoosh "Simple Moving Averages"
"SimpleMovingAverages"
--}

fetchAllRecommendations :: Connection -> IO (Map Day (Set Recommendation))
fetchAllRecommendations conn =
   allRektsByDay . mapMaybe toRecsRekts <$> query_ conn fetchAllRektsQuery

allRektsByDay :: [Recommendation] -> Map Day (Set Recommendation)
allRektsByDay = snarf (Just . (date &&& id))

-- the coin-ids are simply Set.map idx on (Set Recommendation)

-- also: let allcoins = Set.unions (Map.elems dailyRektx)

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

fetchRecs :: Connection -> Day -> IO Recs
fetchRecs conn date =
   Map.fromList . sortOn snd . map (idx &&& call . row)
   <$> fetchRecommendations conn date
