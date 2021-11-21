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
        dateDir "yield-farming/sushi" dt    >>=
        readSushiFarms . (++ "/scrape.txt") >>=
        reportYields "sushi" dt
