module CryptoCoin.CoinMarketCap.Analytics.Trends.Trend where

import Control.Arrow ((&&&))

import qualified Data.ByteString.Char8 as B
import qualified Data.Map as Map
import Data.Time (Day)

import Database.PostgreSQL.Simple (Connection, Query, executeMany)
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.Types

import CryptoCoin.CoinMarketCap.Analytics.Trends.Indicators
import CryptoCoin.CoinMarketCap.Data.TrackedCoin (trackedCoins)

import Data.CryptoCurrency.Types (Idx, IxRow(IxRow))
import Data.CryptoCurrency.Types.Recommendation
import Data.CryptoCurrency.Types.Trend

import Data.LookupTable (LookupTable)
import Data.Time.TimeSeries (today)

import Store.SQL.Connection (withConnection, Database(ECOIN))

-- Okay, so, now let's run all the indicators and update Trend with the
-- new values

trendResult :: Day -> Idx -> TrendResults -> Trend
trendResult tday coinId trendMap =
   let luk = flip Map.lookup trendMap in
   IxRow coinId tday
         (TrendData (luk (SimpleMovingAverage, 50))
                    (luk (SimpleMovingAverage, 200))
                    (luk (ExponentialMovingAverage, 9))
                    (luk (ExponentialMovingAverage, 12))
                    (luk (ExponentialMovingAverage, 26))
                    (luk (MovingAverageConvergenceDivergence, 1))
                    (luk (RelativeStrengthIndex, 1))
                    (luk (OnBalanceVolume, 1)))

instance ToRow TrendData where
   toRow (TrendData s5 s2 e9 e1 e2 m r o) =
      map toField [s5, s2, e9, e1, e2, m, r, o]

storeTrendQuery :: Query
storeTrendQuery = Query . B.pack $ unwords [
   "INSERT INTO trend (cmc_id, for_date, sma_50, sma_200,",
   "ema_9_signal_line, ema_12, ema_26, macd, rsi_14, obv)",
   "VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"]

storeTrends :: Connection -> Day -> LookupTable -> IO ()
storeTrends conn tday tracked =
   let msg = "Storing indicators for " ++ show (length tracked) ++ " e-coins" in
   putStrLn msg                                                             >>
   mapM (sequence . (snd &&& runAllIndicatorsOn conn)) (Map.toList tracked) >>=
   executeMany conn storeTrendQuery . map (uncurry (trendResult tday))      >>
   putStrLn "...done."

go :: IO ()
go = today >>= \tday ->
     withConnection ECOIN (\conn ->
        trackedCoins conn >>= storeTrends conn tday)
