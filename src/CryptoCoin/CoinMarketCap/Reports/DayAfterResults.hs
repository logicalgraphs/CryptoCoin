{-# LANGUAGE OverloadedStrings #-}

module CryptoCoin.CoinMarketCap.Reports.DayAfterResults where

import qualified Data.ByteString.Char8 as B
import Data.Time (Day, addDays)

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.Types (Query(Query))

import Control.Presentation (Univ, explode)

import CryptoCoin.CoinMarketCap.Reports.Table (csvReport)
import CryptoCoin.CoinMarketCap.Utils (geaux)

import Data.CryptoCurrency.Types (Idx, Rank, rank)

import Data.Percentage
import Data.Monetary.USD

-- shows the results of yesterday's recommendations

allRecsQuery :: Query
allRecsQuery = Query . B.pack $ unlines [
   "select b.cmc_id, c.symbol, b.for_date, a.for_date,",
                              "b.rank, a.rank,",
                              "b.quote_price,a.quote_price",
   "from coin_market_cap_daily_listing a",
   "inner join coin_market_cap_daily_listing b on a.cmc_id=b.cmc_id",
   "inner join coin c on c.cmc_id=b.cmc_id",
   "where a.for_date = ? and b.for_date= ?",
   "and a.cmc_id in (select cmc_id from recommendation where for_date=?)",
   "order by b.rank"]

data Result = Result { cmcId :: Idx, sym    :: String,
                       yest, tday           :: Day,
                       yestRank, tdayRank   :: Integer,
                       yestPrice, tdayPrice :: USD }
   deriving (Eq, Ord, Show)

data ResultInput = RI Day Day

instance ToRow ResultInput where
   toRow (RI t y) = [toField t, toField y, toField y]

instance Rank Result where
   rank = yestRank

instance Univ Result where
   explode r =
      let (USD yp) = yestPrice r
          (USD tp) = tdayPrice r
          yr = yestRank r
          tr = tdayRank r in
      [show (cmcId r), sym r,
       show (tday r), show tr, show (USD tp),
       show (yest r), show yr, show (USD yp),
       show (tr - yr), show (P ((tp - yp) / tp))]

instance FromRow Result where
   fromRow = Result <$> field <*> field <*> field <*> field
                                        <*> field <*> field
                                        <*> field <*> field

allResults :: Connection -> Day -> IO [Result]
allResults conn tday =
   let yest = addDays (-1) tday in
   query conn allRecsQuery (RI tday yest)

allResultsAndReport :: Connection -> Day -> IO ()
allResultsAndReport conn date =
   allResults conn date >>= csvReport date "result" rhdrs

rhdrs :: [String]
rhdrs =
   words "id sym today rank price yesterday rank price rank-diff delta-price"

go :: IO ()
go = geaux allResultsAndReport
