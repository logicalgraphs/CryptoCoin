module CryptoCoin.CoinMarketCap.Analytics.Trends.Recommendation where

-- from today's trend data, make buy/sell recommendations

import Database.PostgreSQL.Simple

import Data.CryptoCurrency.Types (Idx)
import Data.CryptoCurrency.Types.Recommendation
import Data.CryptoCurrency.Types.Trend

buySell :: Connection -> Idx -> IO [Recommendation]
buySell conn coinId =
   fetchLastTrend conn coinId >>= \trends ->
   undefined
