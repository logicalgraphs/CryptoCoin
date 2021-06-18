{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE ViewPatterns      #-}

module CryptoCoin.CoinMarketCap.Reports.Results where

{--
So, we got the recommendations, ... but what are the results?

import Control.Arrow ((&&&))

import qualified Data.ByteString.Char8 as B
import Data.Foldable (toList)
import Data.List (intercalate)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tuple (swap)

import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.Types (Query(Query))

import Control.List (weave)
import Control.Map (snarf)


import Data.LookupTable (LookupTable)
import Data.CryptoCurrency.Types (IxRow(IxRow), idx, Idx, row, Indexed, 
           Rank, rank, namei, Named)
import Data.CryptoCurrency.Types.Recommendation
          (Recommendation, RecommendationData(Rekt), call, Call(BUY), 
           fetchRecommendations, Source, toSource)
import Data.CryptoCurrency.Utils (plural, pass)

import Store.SQL.Util.LookupTable (lookupTable)
--}

import Control.Arrow ((&&&))

import qualified Data.ByteString.Char8 as B
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Time (Day, addDays)

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types (Query(Query))

import Control.Presentation

import CryptoCoin.CoinMarketCap.Reports.Recommendation
import CryptoCoin.CoinMarketCap.Reports.Table (csvReport)

import Data.CryptoCurrency.Types (Idx, idx)
import Data.Monetary.USD

import Store.SQL.Connection (withConnection, Database(ECOIN))
import Store.SQL.Util.Indexed (IxValue, val, ix)
import Store.SQL.Util.TaggedTypes
import Store.SQL.Util.Time (latest)

{--
so each coin can have multiples of 

(rec (ind + basis + mb confidence))

coin is: id + sym + name + (today's) price + rank

exchanges is [exchange] -- simple lookup

lookup tables are call, rec(ind x basis x confidence), tracked

the 'lookup table' of coin will be built from the recommendations from today

-- We already have a fetchRecommendations-command.

data CoinRow = CoinRow String String String USD Integer
   deriving (Eq, Ord, Show)

type IxRowCoin = IxRow CoinRow

instance FromRow CoinRow where
   fromRow = CoinRow <$> field <*> field <*> field
                     <*> (dollah <$> field)  <*> field
      where dollah :: Double -> USD
            dollah = USD . toRational

instance Rank CoinRow where
   rank (CoinRow _ _ _ _ r) = r

inSet :: Foldable t => t a -> Only (In [a])
inSet = Only . In . toList

fetchCoinsQuery :: Day -> Query
fetchCoinsQuery date = Query . B.pack $ unlines [
   "SELECT c.cmc_id, d.for_date, c.symbol, c.name, c.slug, d.quote_price, d.rank",
   "FROM coin c",
   "INNER JOIN coin_market_cap_daily_listing d ON d.cmc_id=c.cmc_id",
   "WHERE d.for_date='" ++ show date ++ "' AND c.cmc_id IN ?"]

fetchCoinsInfos :: Foldable t => Connection -> Day -> t Idx
                -> IO (Map Idx IxRowCoin)
fetchCoinsInfos conn date coins =
   Map.fromList . map (idx &&& id)
   <$> query conn (fetchCoinsQuery date) (inSet coins)

>>> today >>= \tday ->
    withConnection ECOIN (\conn -> 
        fetchCoinsInfos conn tday exCoins >>= mapM_ print . Map.toList)
(1376,IxRow 1376 2021-04-22 (CoinRow "NEO" "Neo" "neo" $101.52 21))
(1437,IxRow 1437 2021-04-22 (CoinRow "ZEC" "Zcash" "zcash" $243.95 50))
(1567,IxRow 1567 2021-04-22 (CoinRow "NANO" "Nano" "nano" $8.73 84))
(1772,IxRow 1772 2021-04-22 (CoinRow "STORJ" "Storj" "storj" $1.86 122))
(1856,IxRow 1856 2021-04-22 (CoinRow "DNT" "district0x" "district0x" $0.31 275))
(1966,IxRow 1966 2021-04-22 (CoinRow "MANA" "Decentraland" "decentraland" $1.39 56))
(2099,IxRow 2099 2021-04-22 (CoinRow "ICX" "ICON" "icon" $2.18 80))
(3783,IxRow 3783 2021-04-22 (CoinRow "ANKR" "Ankr" "ankr" $0.14 93))

coinExchangesQuery :: Query
coinExchangesQuery = Query . B.pack $ unwords [
   "SELECT c.cmc_id, ttlk.tracked_type, ttlk.url",
   "FROM coin c",
   "INNER JOIN j_tracked_coin_tracked_type jtctt ON",
                           "jtctt.tracked_coin_id=c.cmc_id",
   "INNER JOIN tracked_type_lk ttlk ON",
                           "ttlk.tracked_type_id=jtctt.tracked_type_id",
   "WHERE c.cmc_id IN ?"]

data Exchange = Exchange Idx String FilePath
   deriving (Eq, Ord, Show)

instance Indexed Exchange where
   idx (Exchange i _ _) = i

instance Named Exchange where namei (Exchange _ n _) = n

instance FromRow Exchange where
   fromRow = Exchange <$> field <*> field <*> field
   
type Exchanges = Map Idx (Set Exchange)

class Site a where
   url :: a -> FilePath

instance Site Exchange where
   url (Exchange _ _ u) = u

coinExchanges :: Foldable t => Connection -> t Idx -> IO Exchanges
coinExchanges conn idxs =
   snarf (return . (idx &&& id))
      <$> query conn coinExchangesQuery (inSet idxs)

exCoins :: [Idx]
exCoins = [1376, 1437, 1966, 2099, 1567, 3783, 1772, 1856]

What does, e.g.:

>>> withConnection ECOIN (\conn -> 
       coinExchanges conn exCoins >>=
       mapM_ print . Map.toList)

return?

(1376,fromList [Exchange {exId = 1376, name = "BINANCE", 
                          url = "https://www.binance.us/en/home"}])
(1437,fromList [Exchange {exId = 1437, name = "COINBASE", 
                          url = "https://www.coinbase.com"}])
(1567,fromList [Exchange {exId = 1567, name = "BINANCE", 
                          url = "https://www.binance.us/en/home"}])
(1772,fromList [Exchange {exId = 1772, name = "BINANCE", 
                          url = "https://www.binance.us/en/home"},
                Exchange {exId = 1772, name = "COINBASE", 
                          url = "https://www.coinbase.com"}])
...

data IndicatorInfo = II String FilePath
   deriving (Eq, Ord, Show)

instance Site IndicatorInfo where
   url (II _ u) = u

instance FromRow IndicatorInfo where
   fromRow = II <$> field <*> field

data TLARow' = TLAR' { ind, base :: String, ii :: IndicatorInfo }
   deriving (Eq, Ord, Show)

instance FromRow TLARow' where
   fromRow = TLAR' <$> field <*> field <*> fromRow

fetchIndicatorInfoQuery :: Query
fetchIndicatorInfoQuery = Query . B.pack $ unlines [
   "SELECT i.indicator, b.basis, i.tla, i.url",
   "FROM indicator_lk i",
   "INNER JOIN basis_lk b ON b.basis_id=i.basis_id"]

type TLAs = Map Source IndicatorInfo

fetchIndicatorInfo :: Connection -> IO TLAs
fetchIndicatorInfo conn =
   Map.fromList . mapMaybe fooey <$> query_ conn fetchIndicatorInfoQuery

fooey :: TLARow' -> Maybe (Source, IndicatorInfo)
fooey (TLAR' ind bas ii) = (,ii) <$> toSource ind bas

>>> withConnection ECOIN (\conn -> fetchIndicatorInfo conn >>= mapM_ print . Map.toList)
(Pat ThreeWhiteKnights,II "TWK" "https://www.investopedia.com/terms/t/three_white_soldiers.asp")
(Pat ThreeBlackCrows,II "TBC" "https://www.investopedia.com/terms/t/three_black_crows.asp")
(Pat AbandonedBaby,II "AB" "https://www.investopedia.com/terms/b/bullish-abandoned-baby.asp")
(Pat TwoBlackGapping,II "TBG" "https://www.investopedia.com/articles/active-trading/092315/5-most-powerful-candlestick-patterns.asp#two-black-gapping")
(Pat EveningStar,II "ES" "https://www.investopedia.com/articles/active-trading/092315/5-most-powerful-candlestick-patterns.asp#evening-star")
(Ind SimpleMovingAverage,II "SMA" "https://www.investopedia.com/terms/s/sma.asp")
(Ind ExponentialMovingAverage,II "EMA" "https://www.investopedia.com/terms/e/ema.asp")
(Ind MovingAverageConvergenceDivergence,II "MACD" "https://www.investopedia.com/terms/m/macd.asp")
(Ind RelativeStrengthIndex,II "RSI" "https://www.investopedia.com/terms/r/rsi.asp")
(Ind OnBalanceVolume,II "OBV" "https://www.investopedia.com/terms/o/onbalancevolume.asp")

data RecRow = RR { coin :: IxRowCoin,
                   wherez :: Set Exchange,
                   indInfos :: TLAs,
                   buys, sells :: Set Recommendation }
   deriving (Eq, Show)

instance Rank RecRow where
   rank = rank . row . coin

type BuySell = (Set Recommendation, Set Recommendation)

buySell :: Set Recommendation -> BuySell
buySell = Set.partition ((== BUY) . call . row)

recRow :: Exchanges -> TLAs -> Map Idx (Set Recommendation) -> IxRowCoin
       -> Maybe RecRow
recRow ex tlas mm coin =
   let coinId = idx coin
       mlu = Map.lookup coinId
   in  mlu ex >>= \e ->
       mlu mm >>=
       return . uncurry (RR coin e tlas) . buySell

recommendations :: Exchanges -> TLAs -> [Recommendation] -> [IxRowCoin]
                -> [RecRow]
recommendations exs tlas (snarf (Just . (idx &&& id)) -> recs) =
   mapMaybe (recRow exs tlas recs)

collateRecommendations :: Connection -> Day -> IO [RecRow]
collateRecommendations conn date =
   fetchRecommendations conn date    >>= \recs ->
   let coinIds = Set.fromList (map idx recs) in
   coinExchanges conn coinIds        >>= \coinExs ->
   fetchIndicatorInfo conn           >>= \tlas ->
   fetchCoinsInfos conn date coinIds >>=
   return . recommendations coinExs tlas recs . Map.elems

>>> today >>= \tday -> withConnection ECOIN (\conn -> 
              collateRecommendations conn (addDays (-1) tday) >>= mapM_ print)
RR {coin = IxRow 1376 2021-04-20 (CoinRow "NEO" "Neo" $110.05 21), 
    wherez = fromList [Exchange {exId = 1376, name = "BINANCE", 
                                 url = "https://www.binance.us/en/home"}], 
    buys = fromList [IxRow 1376 2021-04-20
                           (Rekt {call = BUY, 
                                  source = Pat ThreeWhiteKnights, 
                                  confidence = Just 91.00%})], 
    sells = fromList []}
RR {coin = IxRow 1437 2021-04-20 (CoinRow "ZEC" "Zcash" $230.94 50), 
    wherez = fromList [Exchange {exId = 1437, name = "COINBASE", 
                                 url = "https://www.coinbase.com"}], 
    buys = fromList [IxRow 1437 2021-04-20
                           (Rekt {call = BUY, 
                                  source = Pat ThreeWhiteKnights, 
                                  confidence = Just 91.00%})], 
    sells = fromList []}
...

thdr :: [String]
thdr = words "ID symbol name price rank buys sells exchanges"

pipe :: Foldable t => (a -> Maybe String) -> t a -> String
pipe f = intercalate "|" . mapMaybe f . toList

jexs :: Foldable t => t Exchange -> String
jexs = pipe (pure . namei)

ts' :: Foldable t => TLAs -> t Recommendation -> String
ts' tlas = pipe (\r -> fst <$> tlb tlas r)
--}

data ResultRow = RRR RecRow USD Score
   deriving (Eq, Show)

data Score = WIN | EH | LOSE
   deriving (Eq, Ord, Show)

instance Univ ResultRow where
   explode (RRR rr prc score) = explode rr ++ [show prc, show score]

-- okay, we need to compute today's price for the comparison, then we need
-- to compute the W.

pricesToday :: Foldable t => Connection -> Day -> t Idx -> IO (Map Idx USD)
pricesToday conn tday idxn =
   Map.fromList . map (ix &&& untag . val) <$> p2t conn tday idxn

p2t :: Foldable t => Connection -> Day -> t Idx -> IO [IxValue (TaggedType USD)]
p2t conn tday = query conn (pricesQuery tday) . inSet

pricesQuery :: Day -> Query
pricesQuery date = Query . B.pack $ unlines [
   "SELECT cmc_id, quote_price FROM coin_market_cap_daily_listing",
   "WHERE for_date='" ++ show date ++ "' AND cmc_id IN ?"]

theW :: RecRow -> USD -> ResultRow
theW rr tdayPrice = RRR rr tdayPrice (computeW rr tdayPrice)

computeW :: RecRow -> USD -> Score
computeW rr prc = undefined

rrrFromRr :: Connection -> Day -> [RecRow] -> IO [ResultRow]
rrrFromRr conn tday rrs =
   pricesToday conn tday (map idx rrs) >>= \prices ->
   return (map (uncurry theW)
               (mapMaybe (sequence . (id &&& flip Map.lookup prices . idx)) rrs))

go :: IO ()
go = withConnection ECOIN (\conn -> 
        latest conn "recommendation" "for_date" >>= \tday ->
        collateRecommendations conn tday        >>=
        csvReport tday "recommendation" thdr)
