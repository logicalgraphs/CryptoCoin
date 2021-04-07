{-# LANGUAGE OverloadedStrings #-}

module CryptoCoin.CoinMarketCap.Analytics.Trends.Indicators where

{--
Like the Candlestick patterns, these are the "Trend Trading: The 4 Common
Indicators" outside the candlesticks (focusing more on price, ... sometimes).

--}

{--
Need:

cmc_id, volume_24h, for_date, quote_price

Need: 50 an 200 for sma, 12 and 26 for EMA / MACD, 15 for RSI, 
OBV just needs the previous day and today.
--}

import qualified Data.ByteString.Char8 as B

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Time

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.Types

import CryptoCoin.CoinMarketCap.Analytics.Trends.SimpleMovingAverage (sma)

import Data.CryptoCurrency.Types (PriceVolume)
import Data.CryptoCurrency.Types.Vector (Vector, mkVect, vtake)

import Store.SQL.Connection (withConnection, Database(ECOIN))
import Store.SQL.Util.Indexed

fetchRowsQuery :: Query
fetchRowsQuery = Query . B.pack $ unwords [
  "SELECT cmc_id, for_date, quote_price, volume_24h",
  "FROM coin_market_cap_daily_listing",
  "WHERE cmc_id=?",
  "ORDER BY for_date DESC",
  "LIMIT 200"]

fetchRows :: Connection -> Integer -> IO (Vector PriceVolume)
fetchRows conn coinId = mkVect <$> query conn fetchRowsQuery (Idx coinId)

{--
>>> withConnection ECOIN (\conn -> fetchRows conn 1 >>= mapM print >>= 
          putStrLn . ("There were " ++) . (++ " rows of data.") . show . length)
...
Row {date = 2021-03-10, cmcId = 1, price = 56687.8536484267, vol = 5.70546138680779e10}
Row {date = 2021-03-05, cmcId = 1, price = 47437.3885719626, vol = 4.90061340250151e10}
There were 27 rows of data.
--}

data Indicator = SimpleMovingAverage
               | ExponentialMovingAverage
               | MovingAverageConvergenceDivergence
               | RelativeStrengthIndex
               | OnBalanceVolume
   deriving (Eq, Ord, Show)

type IndicatorA = (Int -> Int, Vector PriceVolume -> Double)
type Indicators = Map Indicator IndicatorA

indicators :: Indicators
indicators = Map.fromList [
   (SimpleMovingAverage, (id, sma)),
   (ExponentialMovingAverage, (succ, ema))]

guardedIndicator :: Vector PriceVolume -> Int -> IndicatorA -> Maybe Double
guardedIndicator v sz (nf, f) = f <$> vtake (nf sz) v

btc :: Indicator -> Int -> IO ()
btc ind i =
   let mbIndA = Map.lookup ind indicators
       mbGI v = mbIndA >>= guardedIndicator v i
   in  withConnection ECOIN (\conn -> fetchRows conn 1 >>= print . mbGI)

{--
>>> btc SimpleMovingAverage 15
Just 56640.03270665875

>>> btc SimpleMovingAverage 200
Nothing
--}
