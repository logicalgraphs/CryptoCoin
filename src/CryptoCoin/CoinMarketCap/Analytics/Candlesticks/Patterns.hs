module CryptoCoin.CoinMarketCap.Analytics.Candlesticks.Patterns where

{--
We take our tracked securities' candlesticks and run them against patterns
to generate buy/sell/hold recommendations.
--}

import Control.Arrow (second, (&&&))
import Control.Monad (liftM)

import qualified Data.ByteString.Char8 as B

import Data.Map (Map)
import qualified Data.Map as Map

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.Types

import CryptoCoin.CoinMarketCap.Analytics.Candlesticks.ThreeWhiteKnights (threeWhiteKnights)
import CryptoCoin.CoinMarketCap.Data.TrackedCoin (trackedCoins)

import Data.CryptoCurrency.Types

import Data.Percentage

import Store.SQL.Connection (withConnection, Database(ECOIN))

data Pattern = ThreeWhiteKnights
             | ThreeLineStrikes
             | ThreeBlackCrows
             | AbandonedBaby
             | TwoBlackGapping
             | EveningStar
   deriving (Eq, Ord, Show)

type Signal = [OCHLV] -> Bool

undef :: Signal      -- the 'not yet' signal
undef = const False

data Call = BUY | SELL
   deriving (Eq, Ord, Show)

data Recommendation = Rec Call Percentage
   deriving (Eq, Ord, Show)

type Patterns = Map Pattern (Signal, Recommendation)

patterns :: Patterns
patterns = Map.fromList [
   (ThreeLineStrikes,  (undef, Rec BUY $ P 83)),
   (TwoBlackGapping,   (undef, Rec SELL $ P 68)),
   (ThreeBlackCrows,   (undef, Rec SELL $ P 78)),
   (EveningStar,       (undef, Rec BUY $ P 72)),  -- really?
   (AbandonedBaby,     (undef, Rec SELL $ P 50)), -- dunno
   (ThreeWhiteKnights, (threeWhiteKnights, Rec BUY $ P 91))]

-- so for some tracked coin, we load in the OCHLV for the last x days
-- and return a set of patterns signaled along with their confidence measures

-- ... somehow we also need to link buy-sell recommendations to the above
-- patterns.

instance FromRow OCHLV where
   fromRow = OCHLV <$> field <*> field <*> field <*> field <*> field
                   <*> field <*> field <*> field

candlesQuery :: Query
candlesQuery = Query . B.pack $ unwords [
   "SELECT cmc_id, for_date, open, close, high, low, adjusted_close, volume",
   "FROM candlesticks WHERE cmc_id=? ORDER BY for_date DESC LIMIT ?"]

candlesFor :: Connection -> Idx -> IO [OCHLV]
candlesFor conn cmcId = query conn candlesQuery (cmcId, 5 :: Integer)

runPatterns :: [OCHLV] -> Patterns -> [(Pattern, Recommendation)]
runPatterns ctx = map (second snd) . filter (run ctx . fst . snd) . Map.toList
   where run = flip ($)

go :: IO ()
go = withConnection ECOIN (\conn ->
   trackedCoins conn >>=
   mapM_ (liftM print . sequence . (id &&& doIt conn . snd)) . Map.toList)
      where doIt conn cmcId = flip runPatterns patterns <$> candlesFor conn cmcId
