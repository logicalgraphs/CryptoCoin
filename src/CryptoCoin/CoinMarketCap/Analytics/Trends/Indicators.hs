{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module CryptoCoin.CoinMarketCap.Analytics.Trends.Indicators where

{--
Like the Candlestick patterns, these are the "Trend Trading: The 4 Common
Indicators" outside the candlesticks (focusing more on price, ... sometimes).

Need:

cmc_id, volume_24h, for_date, quote_price

Need: 50 an 200 for sma, 12 and 26 for EMA / MACD, 15 for RSI, 
OBV just needs the previous day and today.
--}

import Control.Arrow ((&&&), first)

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Maybe (mapMaybe, maybeToList)

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow

import CryptoCoin.CoinMarketCap.Analytics.Trends.SimpleMovingAverage (sma)
import CryptoCoin.CoinMarketCap.Analytics.Trends.ExponentialMovingAverage (ema)
import CryptoCoin.CoinMarketCap.Analytics.Trends.MovingAverageConvergenceDivergence (macdi)
import CryptoCoin.CoinMarketCap.Analytics.Trends.RelativeStrengthIndex (rsi)
import CryptoCoin.CoinMarketCap.Analytics.Trends.OnBalanceVolume (obvi)

import Data.CryptoCurrency.Types (row, Idx)
import Data.CryptoCurrency.Types.PriceVolume
import Data.CryptoCurrency.Types.Trend
import Data.CryptoCurrency.Types.Vector (Vector, vtake)

import Data.LookupTable

import Store.SQL.Connection (withConnection, Database(ECOIN))

data Indicator = SimpleMovingAverage
               | ExponentialMovingAverage
               | MovingAverageConvergenceDivergence
               | RelativeStrengthIndex
               | OnBalanceVolume
   deriving (Eq, Ord, Show)

type PVdom = (Trend, Vector PriceVolume)
type PVctx = ((Trend, Maybe Double), Vector PriceVolume)
type IndicatorBase = (TrendData -> Maybe Double, PVctx -> Double)
type IndicatorA = (Int -> Int, IndicatorBase)
type Indicators = Map (Indicator, Int) IndicatorA

data Temporal = Temporal (Map (Indicator, Int) IndicatorA)
data Eternal = Eternal (Map Indicator IndicatorA)

class LookInto a where
   lookin :: a -> (Indicator, Int) -> Maybe IndicatorA

instance LookInto Temporal where
   lookin (Temporal m) = flip Map.lookup m

instance LookInto Eternal where
   lookin (Eternal m) = flip Map.lookup m . fst

temporal :: Temporal
temporal = Temporal $ Map.fromList [
   ((SimpleMovingAverage, 50), (id, (sma50, sma))),
   ((SimpleMovingAverage, 200), (id, (sma200, sma))),
   ((ExponentialMovingAverage, 9), (succ, (ema9, ema))),
   ((ExponentialMovingAverage, 12), (succ, (ema12, ema))),
   ((ExponentialMovingAverage, 26), (succ, (ema26, ema)))]

eternal :: Eternal
eternal = Eternal $ Map.fromList [
   (MovingAverageConvergenceDivergence, (const 27, (macd, macdi))),
   (RelativeStrengthIndex, (const 15, (rsi14, rsi))),
   (OnBalanceVolume, (const 10, (obv, obvi)))]   -- I DON'T KNOW! :/

data Dependency = T Temporal | E Eternal

instance LookInto Dependency where
   lookin (T t) = lookin t
   lookin (E e) = lookin e

guardedIndicator :: PVdom -> Int -> IndicatorA -> Maybe Double
guardedIndicator (t, v) sz (nf, (tf, f)) =
   f <$> sequence ((id &&& tf . row) t, vtake (nf sz) v)

indies :: [Dependency]
indies = [T temporal, E eternal]

type RunCoin = Indicator -> Int -> IO ()

btc, bnb, eth :: RunCoin  -- the big three
btc = c' 1
eth = c' 1027
bnb = c' 1839

c' :: Idx -> Indicator -> Int -> IO ()
c' = coin' indies

coin' :: LookInto indicators => [indicators] -> Idx -> Indicator -> Int -> IO ()
coin' indies idx ind i =
   let mbIndA = mapMaybe (flip lookin (ind, i)) indies
       mbG v = mbIndA >>= maybeToList . guardedIndicator v i
   in  withConnection ECOIN (\conn -> fetchDomain conn idx >>= print . mbG)

fetchDomain :: Connection -> Integer -> IO PVdom
fetchDomain conn idx =
   (,) <$> fetchLastTrend conn idx <*> fetchPricesVolumes conn idx

fetchCmcIdsQuery :: Query
fetchCmcIdsQuery = "SELECT symbol, cmc_id FROM coin WHERE symbol in ?"

fetchCmcIds :: Connection -> [String] -> IO LookupTable
fetchCmcIds conn coins =
   Map.fromList <$> query conn fetchCmcIdsQuery (Only (In coins))

runAllIndicatorsOns :: [String] -> IO ()
runAllIndicatorsOns coins =
   withConnection ECOIN (\conn ->
      fetchCmcIds conn coins >>= mapM_ (runAllIndicatorsOn conn) . Map.toList)

runAllIndicatorsOn :: Connection -> (String, Integer) -> IO ()
runAllIndicatorsOn conn (sym, idx) =
   putStrLn ("For e-coin " ++ sym ++ " (CMC ID: " ++ show idx ++ "):") >>
   fetchDomain conn idx >>=
   flip mapM_ (pair indies) . runIndicator

runIndicator :: PVdom -> ((Indicator, Int), IndicatorA) -> IO ()
runIndicator dom ((ind, i), r) =
   let res = guardedIndicator dom i r in
   putStrLn ("   " ++ show ind ++ " (" ++ show i ++ "): " ++ mbshow res)

mbshow :: Show a => Maybe a -> String
mbshow Nothing = "--"
mbshow (Just a) = show a

pair :: [Dependency] -> [((Indicator, Int), IndicatorA)]
pair = concat . map (p' 1)

p' :: Int -> Dependency -> [((Indicator, Int), IndicatorA)]
p' _ (T (Temporal t)) = Map.toList t
p' i (E (Eternal e)) = map (first (,i)) (Map.toList e)

{--
>>> btc ExponentialMovingAverage 12
[59134.597811008054]

>>> btc SimpleMovingAverage 200
[]

and for the ones where the dates are preset:

>>> btc MovingAverageConvergenceDivergence 26
[1411.982019920164]

>>> btc RelativeStrengthIndex 234234  -- because days don't matter
[48.53384918758772]

Bitcoin is just chugging along, isn't it.

>>> btc OnBalanceVolume 10
[-8.32027697067179e10]

But may be oversold?

And, to run all the indicators:

>>> runAllIndicatorsOns (words "BTC ETH BNB")
For e-coin BNB (CMC ID: 1839):
   SimpleMovingAverage (50): --
   SimpleMovingAverage (200): --
   ExponentialMovingAverage (9): 441.1934542263441
   ExponentialMovingAverage (12): 414.90682992567207
   ExponentialMovingAverage (26): 339.2260297583855
   MovingAverageConvergenceDivergence (1): 81.82557999904827
   RelativeStrengthIndex (1): 72.4312195763753
   OnBalanceVolume (1): -2.684611258097431e10
For e-coin BTC (CMC ID: 1):
   SimpleMovingAverage (50): --
   SimpleMovingAverage (200): --
   ExponentialMovingAverage (9): 59097.80574823082
   ExponentialMovingAverage (12): 59134.597811008054
   ExponentialMovingAverage (26): 57798.71974225892
   MovingAverageConvergenceDivergence (1): 1411.982019920164
   RelativeStrengthIndex (1): 48.53384918758772
   OnBalanceVolume (1): -8.32027697067179e10
For e-coin ETH (CMC ID: 1027):
   SimpleMovingAverage (50): --
   SimpleMovingAverage (200): --
   ExponentialMovingAverage (9): 2110.11014698635
   ExponentialMovingAverage (12): 2068.89789499771
   ExponentialMovingAverage (26): 1906.5550178257406
   MovingAverageConvergenceDivergence (1): 178.09325184955037
   RelativeStrengthIndex (1): 63.44754316660384
   OnBalanceVolume (1): -2.4423049124036507e10
--}
