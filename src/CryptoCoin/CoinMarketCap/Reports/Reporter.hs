{-# LANGUAGE ViewPatterns #-}

module CryptoCoin.CoinMarketCap.Reports.Reporter where

{--
Okay, report.sh downloads the JSON of listings for us to process. Here, we
report on the files processed.
--}

import Data.Aeson (decode)

import qualified Data.ByteString.Lazy.Char8 as BL

import Data.List (sortOn, splitAt)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Data.Time (Day)
import Data.Time.Calendar (toGregorian)

import Data.CryptoCurrency.Types (rank)

import CryptoCoin.CoinMarketCap.Types (NewCoins, NewCoinsCtx, MetaData(MetaData), coin)
import CryptoCoin.CoinMarketCap.Reports.Table (header, report, newCoins, plural)

import Store.SQL.Util.Indexed (IxValue(IxV))

ranking :: Day -> NewCoinsCtx -> IO NewCoins
ranking date (IxV _ md@(MetaData status ecoins), newsies) =
   header date                                                     >>
   report md (take 10 (sortOn rank . map coin $ Map.elems ecoins)) >>
   newCoins md newsies

{--
>>> let date = (read "2021-02-22") :: Day
>>> ranking date coins
<p>The top-10 e-coins for 2021-02-22 (ranked by 
   <a href='https://coinmarketcap.com'>coinmarketcap.com</a> ) are:</p>
   <table border="1">
    <tr><th align="left">Rank</th><th align="left">Name</th>
        <th align="left">Symbol</th><th align="left">Type</th>
    </tr>
    <tr><td>1</td><td>Bitcoin</td><td>BTC</td><td>Coin</td></tr>
...
--}

tweet :: Day -> NewCoins -> IO ()
tweet today (coins, tokens) =
   let (yr, mos, _) = toGregorian today
       showOught m = (if m < 10 then ('0':) else id) (show m)
       url = concat ["http://logicalgraphs.blogspot.com/", show yr, "/",
                     showOught mos, "/top-10-e-coins-for-"]
       day = show today
       urlday = url ++ day ++ ".html"
       ncoins = length coins
       ntokens = length tokens
   in  putStrLn ("The top-10 e-coins for " ++ day ++ coinsNtokens ncoins ntokens
                 ++ " are archived at " ++ urlday ++ " #cryptocurrency ")

-- Looking at the below arithmetic, I can see that George Boole was onto 
-- something!

coinsNtokens :: Int -> Int -> String
coinsNtokens c t = c' (c + t) c t

c' :: Int -> Int -> Int -> String
c' 0 _ _ = ""
c' _ c t =
    " with" ++ n "coin" c ++ connective (c * t) ++ n "token" t ++ " for today"

connective :: Int -> String
connective 0 = ""
connective _ = " and"

n :: String -> Int -> String
n _ 0 = ""
n typ c = ' ':show c ++ " new " ++ typ ++ plural c

title :: Day -> IO ()
title = putStrLn . unwords . ("Top-10 E-coins for":) . return . show 
