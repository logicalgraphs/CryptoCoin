{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module CryptoCoin.CoinMarketCap.Reports.Utils where

-- we just have utils everywhere, don't we. BUT (as any Prologer will tell you)
-- IT'S FOR A GOOD CLAUSE! :<

import Data.List (intercalate)
import Data.Time (Day, toGregorian)

import Database.PostgreSQL.Simple

import Store.SQL.Util.TaggedTypes

tweet :: Day -> String -> String -> IO ()
tweet today route message =
   let (yr, mos, _) = toGregorian today
       showOught m = (if m < 10 then ('0':) else id) (show m)
       url = intercalate "/" ["http://logicalgraphs.blogspot.com", show yr,
                              showOught mos, route]
       day = show today
       urlday = url ++ day ++ ".html"
   in  putStrLn (unwords [message, "for", day, "are archived at",
                          urlday, "#cryptocurrency"])

connective :: Int -> String
connective 0 = ""
connective _ = " and"

-- for the reports, 'today' is whatever is last in the data-sets.

today :: Connection -> IO Day
today conn =
   untag <$> tday conn "SELECT max(for_date) FROM coin_market_cap_daily_listing"

tday :: Connection -> Query -> IO (TaggedType Day)
tday conn qury = head <$> query_ conn qury
