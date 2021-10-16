{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module CryptoCoin.CoinMarketCap.Analytics.Candlesticks.Patterns where

{--
We take our tracked securities' candlesticks and run them against patterns
to generate buy/sell/hold recommendations.
--}

import Control.Arrow (second, (&&&))
import Control.Monad (void)

import qualified Data.ByteString.Char8 as B

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Time (Day)

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types

import CryptoCoin.CoinMarketCap.Analytics.Candlesticks.ThreeWhiteKnights (twk)
import CryptoCoin.CoinMarketCap.Analytics.Candlesticks.ThreeLineStrike (tls)
import CryptoCoin.CoinMarketCap.Analytics.Candlesticks.TwoBlackGapping (tbg)
import CryptoCoin.CoinMarketCap.Analytics.Candlesticks.ThreeBlackCrows (tbc)
import CryptoCoin.CoinMarketCap.Analytics.Candlesticks.AbandonedBaby (ab)
import CryptoCoin.CoinMarketCap.Analytics.Candlesticks.EveningStar (es)
import CryptoCoin.CoinMarketCap.Data.TrackedCoin (trackedCoins)
import CryptoCoin.Utils (geaux)

import Data.CryptoCurrency.Types
import Data.CryptoCurrency.Types.OCHLV
import Data.CryptoCurrency.Types.Recommendation
import Data.CryptoCurrency.Types.Vector

import Data.LookupTable (LookupTable)

import Data.Percentage

import Data.Time.TimeSeries (today)

import Store.SQL.Connection (withConnection, Database(ECOIN))
import Store.SQL.Util.LookupTable (lookupTable, lookupTableFrom)

type Signal = Vector OCHLV -> Bool

undef :: Signal      -- the 'not yet' signal
undef = const False

data Rec' = Rec Call Percentage
   deriving Show

type Patterns = Map Pattern (Signal, Rec')

patterns :: Patterns
patterns = Map.fromList [
   (ThreeLineStrike,   (tls, Rec BUY $ P 0.83)),
   (TwoBlackGapping,   (tbg, Rec SELL $ P 0.68)),
   (ThreeBlackCrows,   (tbc, Rec SELL $ P 0.78)),
   (EveningStar,       (es, Rec SELL $ P 0.72)),
   (AbandonedBaby,     (ab, Rec BUY $ P 0.50)),
   (ThreeWhiteKnights, (twk, Rec BUY $ P 0.91))]

runPatterns :: Vector OCHLV -> Patterns -> [(Pattern, Rec')]
runPatterns ctx = map (second snd) . filter (run ctx . fst . snd) . Map.toList
   where run = flip ($)

candlesAll :: Connection -> LookupTable -> IO [(Idx, (Pattern, Rec'))]
candlesAll conn trackedCoins =
   let sl :: Foldable f => f a -> String
       sl = show . length in
   putStrLn ("Running " ++ sl patterns ++ " candlestick patterns for "
          ++ sl trackedCoins ++ " tracked coins.")                >>
   mapM (sequence' . (id &&& doIt conn)) (Map.elems trackedCoins) >>=
   printP . concat
      where doIt conn cmc = flip runPatterns patterns <$> candlesFor conn cmc
            printP x = mapM_ print x >> return x
            sequence' (idx, mlist) = mlist >>= return . sequence . (idx,)

toRec :: Day -> (Idx, (Pattern, Rec')) -> Recommendation
toRec d (i, (pat, (Rec c p))) = IxRow i d (Rekt c (Pat pat) (Just p))

computeAndStoreCandlestickRecommendations :: Connection -> Day -> IO ()
computeAndStoreCandlestickRecommendations conn tday =
   lookupTable conn "call_lk"   >>= \callLk ->
   lookupInds conn              >>= \indLk ->
   trackedCoins conn            >>=
   candlesAll conn              >>=
   insertRecommendations conn callLk indLk . map (toRec tday)

go :: IO ()
go = geaux computeAndStoreCandlestickRecommendations
