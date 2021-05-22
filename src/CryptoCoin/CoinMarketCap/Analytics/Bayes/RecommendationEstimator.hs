{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module CryptoCoin.CoinMarketCap.Analytics.Bayes.RecommendationEstimator where

{--
We take our prior recommendations, measure their performance, to give confidence
to the recommendations listed today.

Question: what happens with multiple recommendations for a coin? EEP!

This means we run this analysis AFTER we make the recommendations for today.

AND the recommendations before today serve as the training set, and the
recommendations today are to be classified.
--}

import Control.Arrow ((&&&), second)
import qualified Data.ByteString.Char8 as B
import Data.Foldable (toList)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Time (Day)
import Data.Tuple (swap)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.Types

import CryptoCoin.CoinMarketCap.Data.TrackedCoin (trackedCoins)
import Data.CryptoCurrency.Types
import Data.CryptoCurrency.Types.Recommendation
import Data.LookupTable (lookdown, LookDown)
import Data.Monetary.USD
import Data.Time.TimeSeries (today)

import Store.SQL.Util.Indexed (IxValue(IxV), val)

-- the idea is to provide a context of the price of the ecoin of the
-- recommendation then to the price today.

-- This looks like a three-phased approach:
-- I Recs -> II priced recs -> III price delta recs

-- I fetch the recommendations: fetchAllRecommendations, ... obviously

-- II convert the recommendations to recommendation-price for each day

type RecPrice = (Recommendation, USD)

instance FromRow USD where fromRow = USD <$> field

dayPricesQuery :: Day -> Query
dayPricesQuery date = Query . B.pack $ unlines [
   "SELECT cmc_id,quote_price FROM coin_market_cap_daily_listing",
   "WHERE for_date='" ++ show date ++ "' AND cmc_id IN ?"]

pricesFor' :: Connection -> Day -> [Idx] -> IO [IxValue USD]
pricesFor' conn = query conn . dayPricesQuery

instance Indexed (IxValue a) where idx (IxV i _) = i

pricesFor :: Foldable t => Connection -> Day -> t Idx -> IO (Map Idx USD)
pricesFor conn date coinIds =
   Map.fromList . map (idx &&& val) <$> pricesFor' conn date (toList coinIds)

priceRecommendations :: Foldable t => Connection -> Day -> LookDown String
                     -> t Recommendation -> IO (Map Symbol RecPrice)
priceRecommendations conn date coinsLd
                     (Map.fromList . map (idx &&& id) . toList -> recs) =
   pricesFor conn date (Map.keys recs) >>= \coinPrices ->
   let flipLk = flip Map.lookup
       liftTuple (mba, mbb) = (,) <$> mba <*> mbb
       mkRecPrice coinPrices = sequence . second (flipLk coinPrices) . swap in
   return $ Map.fromList
      (mapMaybe (liftTuple . (flipLk coinsLd . fst &&& mkRecPrice coinPrices))
                (Map.toList recs))

-- III get all the prices for all the coins, then do a price diff/recommendation

-- recall that P(Rec) is informed by n-tracked-coins for that day (or any day)
-- n-tracked-coins can be extracted from trackedCoins
