module CryptoCoin.CoinMarketCap.Utils where

-- just a li'l sumthin to help our suite of apps get going

import Data.Char (ord)
import Data.Time (Day)

import Database.PostgreSQL.Simple (Connection)

import System.Environment (getEnv)

import Data.Time.TimeSeries (today)
import Store.SQL.Connection (withConnection, Database(ECOIN))

geaux :: (Connection -> Day -> IO a) -> IO ()
geaux fn = today >>= withConnection ECOIN . flip fn

dateDir :: FilePath -> Day -> IO FilePath
dateDir dir date =
  (++ "/data-files/" ++ dir ++ ('/':show date)) <$> getEnv "CRYPTOCOIN_DIR"

----- UTF8 file cleaning (BEFORE JSONing, I'll note right here) ---------------

sanitize :: String -> String
sanitize = map unicodeSubstitution

unicodeSubstitution :: Char -> Char
unicodeSubstitution = us . id <*> ord

us :: Char -> Int -> Char
us c o | o == 164 = '$'
       | o == 304 = 'I'
       | o == 333 = 'o'
       | o == 351 = 'S'
       | o == 964 = 't'  -- tau, actually
       | o == 932 = 'T'  -- Tau, actually
       | o >  127 = '*'  -- I dunno
       | otherwise = c 
