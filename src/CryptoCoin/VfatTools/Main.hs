module CryptoCoin.VfatTools.Main where

-- run the programs: any yield farms, then the Sushi special case

import Data.Time (Day)

import CryptoCoin.Utils (dateDir)

import CryptoCoin.VfatTools.Report (reportYields)
import CryptoCoin.VfatTools.Scanners.PlainOl (readFarms)
import CryptoCoin.VfatTools.Scanners.Sushi (readSushiFarms)

-- 1HaskellADay imports

import Data.Time.TimeSeries (today, yest)

sushi, sushi' :: IO ()
sushi = today >>= sushi''
sushi' = yest >>= sushi''

sushi'' :: Day -> IO ()
sushi'' dt =
   mkFullPath "sushi" dt >>= readSushiFarms >>= reportYields "sushi" dt

go, gon :: String -> IO ()
go coin = today >>= goes coin
gon coin = yest >>= goes coin

goes :: String -> Day -> IO ()
goes coin dt = mkFullPath coin dt >>= readFarms >>= reportYields coin dt

mkFullPath :: String -> Day -> IO String
mkFullPath coin dt =
   (++ "/scrape.txt") <$> dateDir ("yield-farming/" ++ coin) dt
