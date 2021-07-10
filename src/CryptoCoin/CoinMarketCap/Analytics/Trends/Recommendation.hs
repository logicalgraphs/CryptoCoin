{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module CryptoCoin.CoinMarketCap.Analytics.Trends.Recommendation where

-- from today's trend data, make buy/sell recommendations

import qualified Data.ByteString.Char8 as B

import qualified Data.Map as Map

import Data.Maybe (listToMaybe, mapMaybe)
import Data.Time (Day, addDays)

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types

import Control.Logic.Frege ((-|))

import CryptoCoin.CoinMarketCap.Data.TrackedCoin (trackedCoins)
import CryptoCoin.CoinMarketCap.Utils (geaux)

import Data.CryptoCurrency.Types (Idx, IxRow(IxRow), row)
import Data.CryptoCurrency.Types.Recommendation
import Data.CryptoCurrency.Types.Trend (fetchTrends, Trend, sma50, sma200,
             macd, rsi14, ema9)
import Data.CryptoCurrency.Utils (plural, toBe, pass)

import Data.Time.TimeSeries (today)

import Store.SQL.Connection (withConnection, Database(ECOIN))
import Store.SQL.Util.LookupTable (lookupTableFrom, lookupTable)

type RunRec = Trend -> Maybe Trend -> Maybe Recommendation
type RunRec' = Trend -> Trend -> Maybe Recommendation

buySell :: Connection -> Day -> Idx -> IO [Recommendation]
buySell conn tday coinId =
   patterns <$> fetchTrends conn (addDays (-3) tday) coinId 2 >>= pass reportOn

reportOn :: [Recommendation] -> IO ()
reportOn [] = return ()
reportOn ls@(IxRow i _ _:r) =
   let trndz = succ (length r) in
   putStrLn (concat ["There", toBe trndz, show trndz,
                      " trend indicator", plural trndz,
                      " for ", show i])

patterns :: [Trend] -> [Recommendation]
patterns [] = []
patterns (t0:tz) =
   let prims = [goldenCross', dethCross', risingMacd', fallingMacd'] in
   mapMaybe (runRec t0 tz) ([rsi70, rsi30] ++ map liftR prims)

runRec :: Trend -> [Trend] -> RunRec -> Maybe Recommendation
runRec tday yests fn = fn tday (listToMaybe yests)

mkRec :: Trend -> Call -> Indicator -> Recommendation
mkRec (IxRow ix tday _) c ind = IxRow ix tday (Rekt c (Ind ind) Nothing)

liftR :: RunRec' -> RunRec
liftR fn t0 mbt1 = mbt1 >>= fn t0

-- sma

goldenCross', dethCross' :: RunRec'
goldenCross' t@(IxRow _ _ tr) (IxRow _ _ yest) =
   sma50 tr >>= \sma50tr ->
   sma50 yest >>= \sma50yest ->
   sma200 tr >>= \sma200tr ->
   sma200 yest >>= \sma200yest ->
   (sma50tr > sma200tr) && (sma50yest < sma200tr)
      -| mkRec t BUY SimpleMovingAverage

dethCross' t@(IxRow _ _ tr) (row -> yest) =
   sma50 tr >>= \sma50tr ->
   sma50 yest >>= \sma50yest ->
   sma200 tr >>= \sma200tr ->
   sma200 yest >>= \sma200yest ->
   (sma50tr < sma200tr) && (sma50yest > sma200tr)
      -| mkRec t SELL SimpleMovingAverage

-- macd

risingMacd', fallingMacd' :: RunRec'
risingMacd' t@(IxRow _ _ tr) (row -> yest) =
   macd tr   >>= \macdtr ->
   ema9 tr   >>= \ema9tr ->
   macd yest >>= \macdyest ->
   ema9 yest >>= \ema9yest ->
   (macdyest < ema9yest) && (macdtr > ema9tr)
      -| mkRec t BUY MovingAverageConvergenceDivergence

fallingMacd' t@(IxRow _ _ tr) (row -> yest) =
   macd tr   >>= \macdtr ->
   ema9 tr   >>= \ema9tr ->
   macd yest >>= \macdyest ->
   ema9 yest >>= \ema9yest ->
   (macdyest > ema9yest) && (macdtr < ema9tr)
      -| mkRec t SELL MovingAverageConvergenceDivergence

-- rsa

rsi70, rsi30 :: RunRec
rsi70 t = const $ rsi14 (row t) >>= \rsi14t ->
   rsi14t > 70 -| mkRec t SELL RelativeStrengthIndex
rsi30 t = const $ rsi14 (row t) >>= \rsi14t ->
   rsi14t < 30 -| mkRec t BUY RelativeStrengthIndex

-- not gonna touch On Balance Volume just now.
-- ... but it will be useful for bayesian analysis down the road, maybe?

-- runners

-- so we take the above patterns and run them on the tracked coins, returning
-- a set of recommendations, which we save to the data-store.

computeAndStoreRecommendations :: Connection -> Day -> IO ()
computeAndStoreRecommendations conn date =
   lookupInds conn                      >>= \indLk ->
   lookupTable conn "call_lk"           >>= \callLk ->
   trackedCoins conn                    >>=
   mapM (buySell conn date) . Map.elems >>=
   insertRecommendations conn callLk indLk . concat

go :: IO ()
go = geaux computeAndStoreRecommendations

{--
Here's a fun delete statement:

DELETE FROM
   trend a
     USING trend b
   WHERE
      a.trend_id > b.trend_id
      AND a.cmc_id = b.cmc_id
      AND a.for_date = b.for_date

... deletes duplicate rows of data
--}
