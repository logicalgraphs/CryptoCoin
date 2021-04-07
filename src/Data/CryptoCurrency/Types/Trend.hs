module Data.CryptoCurrency.Types.Trend where

import Control.Monad (mplus, MonadPlus)
import qualified Data.ByteString.Char8 as B

import Data.Maybe (listToMaybe, fromMaybe)

import Data.Monoid ((<>))

import Data.Time (Day, addDays)

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types
import Database.PostgreSQL.Simple.FromRow

import Control.Logic.Frege ((-|))

import Data.CryptoCurrency.Types (IxRow(IxRow), Idx)

import Data.Time.TimeSeries (today)

import Store.SQL.Connection (withConnection, Database(ECOIN))

(<+>) :: MonadPlus m => m a -> m a -> m a
(<+>) = mplus

-- Trend --------------------------------------------------------------

data TrendData =
   TrendData { sma50, sma200, ema9, ema12, ema26,
               macd, rsi14, obv :: Maybe Double }
      deriving (Eq, Ord, Show)

instance Monoid TrendData where
   mempty = TrendData Nothing Nothing Nothing Nothing
                      Nothing Nothing Nothing Nothing
   (TrendData s150 s1200 e19 e112 e126 m1 r114 o1)
      `mappend` (TrendData s250 s2200 e29 e212 e226 m2 r214 o2) =
        TrendData (s250 <+> s150) (s2200 <+> s1200) (e29 <+> e19)
                  (e212 <+> e112) (e226 <+> e126)
                  (m2 <+> m1) (r214 <+> r114) (o2 <+> o1)

instance Monoid a => Monoid (IxRow a) where
   mempty = undefined
   (IxRow i0 _d0 a) `mappend` (IxRow i1 d1 b) =
      if i0 == i1 then IxRow i1 d1 (a <> b) else mempty

type Trend = IxRow TrendData

instance FromRow TrendData where
   fromRow = TrendData <$> field <*> field <*> field <*> field
                       <*> field <*> field <*> field <*> field

fetchTrendQuery :: Query
fetchTrendQuery = Query . B.pack $ unwords [
   "SELECT cmc_id, for_date, sma_50, sma_200, ema_9_signal_line, ema_12,",
   "ema_26, macd, rsi_14, obv",
   "FROM trend",
   "WHERE for_date > ? AND cmc_id=?",
   "ORDER BY for_date DESC",
   "LIMIT ?"]

fetchTrends :: Connection -> Day -> Idx -> Integer -> IO [Trend]
fetchTrends conn date idx lmt = query conn fetchTrendQuery (date, idx, lmt)

-- now: if there are no trends, do we return an null-trend singleton? yes.

fetchLastTrend :: Connection -> Idx -> IO Trend
fetchLastTrend conn idx =
   today                                      >>= \tday ->
   fetchTrends conn (addDays (-2) tday) idx 1 >>=
   return . fromMaybe (IxRow idx (addDays (-1) tday) mempty) . listToMaybe

{--
>>> withConnection ECOIN (\conn -> fetchLastTrend conn 1 >>= print)
IxRow 1 2021-04-06
      (TrendData {sma50 = Nothing, sma200 = Nothing, ema9 = Nothing,
                  ema12 = Nothing, ema26 = Nothing, macd = Nothing,
                  rsi14 = Nothing, obv = Nothing})
--}

updateWith :: Trend -> Trend -> Trend
updateWith = mappend
