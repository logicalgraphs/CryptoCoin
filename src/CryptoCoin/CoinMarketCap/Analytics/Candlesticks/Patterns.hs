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
import Database.PostgreSQL.Simple.Types

import CryptoCoin.CoinMarketCap.Analytics.Candlesticks.ThreeWhiteKnights (threeWhiteKnights)
import CryptoCoin.CoinMarketCap.Data.TrackedCoin (trackedCoins)

import Data.CryptoCurrency.Types
import Data.CryptoCurrency.Types.OCHLV
import Data.CryptoCurrency.Types.Recommendation

import Data.LookupTable (LookupTable)

import Data.Percentage

import Store.SQL.Connection (withConnection, Database(ECOIN))

type Signal = [OCHLV] -> Bool

undef :: Signal      -- the 'not yet' signal
undef = const False

data Rec' = Rec Call Percentage
   deriving Show

type Patterns = Map Pattern (Signal, Rec')

patterns :: Patterns
patterns = Map.fromList [
   (ThreeLineStrike,  (undef, Rec BUY $ P 83)),
   (TwoBlackGapping,   (undef, Rec SELL $ P 68)),
   (ThreeBlackCrows,   (undef, Rec SELL $ P 78)),
   (EveningStar,       (undef, Rec BUY $ P 72)),  -- really?
   (AbandonedBaby,     (undef, Rec SELL $ P 50)), -- dunno
   (ThreeWhiteKnights, (threeWhiteKnights, Rec BUY $ P 91))]

runPatterns :: [OCHLV] -> Patterns -> [(Pattern, Rec')]
runPatterns ctx = map (second snd) . filter (run ctx . fst . snd) . Map.toList
   where run = flip ($)

candlesAll :: Connection -> LookupTable -> IO ()
candlesAll conn trackedCoins =
   let sl :: Foldable f => f a -> String
       sl = show . length in
   putStrLn ("Running " ++ sl patterns ++ " candlestick patterns for "
          ++ sl trackedCoins ++ " tracked coins.") >>
   mapM_ (liftM print . sequence . (id &&& doIt conn . snd))
         (Map.toList trackedCoins)
      where doIt conn cmc = flip runPatterns patterns <$> candlesFor conn cmc

go :: IO ()
go = withConnection ECOIN (\conn -> trackedCoins conn >>= candlesAll conn)
