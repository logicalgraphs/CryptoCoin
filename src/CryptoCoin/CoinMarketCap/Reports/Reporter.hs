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
   let url = "http://logicalgraphs.blogspot.com/2021/03/top-10-e-coins-for-"
       day = show today
       urlday = url ++ day ++ ".html"
       ncoins = length coins
       ntokens = length tokens
   in  putStrLn (unwords ["The top-10 e-coins for", day, "with",
                          show ncoins, "new coin" ++ plural ncoins, "and",
                          show ntokens, "new token" ++ plural ntokens, "for",
                          "today are archived at", urlday, "#cryptocurrency "])

title :: Day -> IO ()
title = putStrLn . unwords . ("Top-10 E-coins for":) . return . show 
